const std = @import("std");
const builtin = @import("builtin");

const graphics = @import("graphics.zig");

pub const c = @cImport ({
	@cInclude("glad/glad.h");
	@cInclude("GLFW/glfw3.h");
	@cInclude("stb/stb_image_write.h");
});

const initialKeyCooldown: i128 = 300_000_000;
const consecutiveKeyCooldown: i128 = 50_000_000;

pub var width: u31 = 1920;
pub var height: u31 = 1080;
const imageWidth: u31 = 192;
const imageHeight: u31 = 108;
const outputScale: u31 = 10;

const Color = graphics.Color;

var global_gpa = std.heap.GeneralPurposeAllocator(.{.thread_safe=true}){};
const allocator: std.mem.Allocator = global_gpa.allocator();

const Image = extern struct {
	imageData: [imageWidth*imageHeight]u8 = [_]u8{0}**(imageWidth * imageHeight),
};

var copyBuffer: []u8 = "";
var copyWidth: u31 = 0;
var copyHeight: u31 = 0;

const CommandType = enum {
	changePixel,
	changeColor,
	fillRect,
	paste,
};

const ChangePixelCommand = struct {
	x: u32,
	y: u32,
	oldColor: u8,
	newColor: u8,
	fn do(posX: u31, posY: u31, frame: *AnimationFrame) void {
		const command = ChangePixelCommand{
			.x = posX,
			.y = posY,
			.oldColor = frame.image.imageData[@intCast(posX + posY*imageWidth)],
			.newColor = currentColor
		};
		if(command.oldColor == command.newColor) return;
		command.redo(frame);
	}
	fn redo(command: ChangePixelCommand, frame: *AnimationFrame) void {
		frame.image.imageData[@intCast(command.x + command.y*imageWidth)] = command.newColor;
		frame.undoBuffer.append(Command{
			.changePixel = command
		}) catch unreachable;
	}
	fn undo(command: ChangePixelCommand, frame: *AnimationFrame) void {
		frame.image.imageData[@intCast(command.x + command.y*imageWidth)] = command.oldColor;
		frame.redoBuffer.append(Command{
			.changePixel = command
		}) catch unreachable;
	}
};

const ChangeColorCommand = struct {
	idx: u8,
	oldColor: Color,
	newColor: Color,
	fn do(newColor: Color, frame: *AnimationFrame) void {
		const command = ChangeColorCommand{
			.idx = currentColor,
			.oldColor = frame.palette[currentColor],
			.newColor = newColor
		};
		if(std.meta.eql(command.oldColor, command.newColor)) return;
		command.redo(frame);
	}
	fn redo(command: ChangeColorCommand, frame: *AnimationFrame) void {
		frame.palette[command.idx] = command.newColor;
		if(frame.undoBuffer.items.len != 0 and frame.undoBuffer.items[frame.undoBuffer.items.len - 1] == .changeColor and frame.undoBuffer.items[frame.undoBuffer.items.len - 1].changeColor.idx == command.idx) {
			frame.undoBuffer.items[frame.undoBuffer.items.len - 1].changeColor.newColor = command.newColor;
		} else {
			frame.undoBuffer.append(Command{
				.changeColor = command
			}) catch unreachable;
		}
	}
	fn undo(command: ChangeColorCommand, frame: *AnimationFrame) void {
		std.log.info("{}, {}", .{command.oldColor.r, frame.palette[command.idx].r});
		frame.palette[command.idx] = command.oldColor;
		frame.redoBuffer.append(Command{
			.changeColor = command
		}) catch unreachable;
	}
};

const FillRectCommand = struct {
	x: u32,
	y: u32,
	width: u32,
	height: u32,
	oldImage: []u8,
	newColor: u8,
	fn do(frame: *AnimationFrame) void {
		var command = FillRectCommand{
			.x = @min(selectionStartX.?,  selectedX),
			.y = @min(selectionStartY.?,  selectedY),
			.width = @max(selectionStartX.?, selectedX) - @min(selectionStartX.?,  selectedX),
			.height = @max(selectionStartY.?, selectedY) - @min(selectionStartY.?,  selectedY),
			.oldImage = undefined,
			.newColor = currentColor
		};
		var isRepeat = true;
		var dx: u31 = 0;
		outer: while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				if(command.newColor != frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)]) {
					isRepeat = false;
					break :outer;
				}
			}
		}
		if(isRepeat) return;
		command.oldImage = allocator.alloc(u8, command.width*command.height) catch unreachable;
		command.redo(frame);
	}
	fn redo(command: FillRectCommand, frame: *AnimationFrame) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				command.oldImage[dx + command.width*dy] = frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)];
				frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)] = command.newColor;
			}
		}
		frame.undoBuffer.append(Command{
			.fillRect = command
		}) catch unreachable;
	}
	fn undo(command: FillRectCommand, frame: *AnimationFrame) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)] = command.oldImage[dx + command.width*dy];
			}
		}
		frame.redoBuffer.append(Command{
			.fillRect = command
		}) catch unreachable;
	}
};

const PasteCommand = struct {
	x: u32,
	y: u32,
	width: u32,
	height: u32,
	oldImage: []u8,
	newImage: []u8,
	fn do(frame: *AnimationFrame) void {
		var command = PasteCommand{
			.x = selectedX,
			.y = selectedY,
			.width = @min(copyWidth, imageWidth - selectedX),
			.height = @min(copyHeight, imageHeight - selectedY),
			.oldImage = undefined,
			.newImage = undefined
		};
		var isRepeat = true;
		var dx: u31 = 0;
		outer: while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				if(copyBuffer[dx + dy*copyWidth] != frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)]) {
					isRepeat = false;
					break :outer;
				}
			}
		}
		if(isRepeat) return;
		command.oldImage = allocator.alloc(u8, command.width*command.height) catch unreachable;
		command.newImage = allocator.alloc(u8, command.width*command.height) catch unreachable;
		dx = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				command.newImage[dx + dy*command.width] = copyBuffer[dx + dy*copyWidth];
			}
		}
		command.redo(frame);
	}
	fn redo(command: PasteCommand, frame: *AnimationFrame) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				command.oldImage[dx + command.width*dy] = frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)];
				frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)] = command.newImage[dx + command.width*dy];
			}
		}
		frame.undoBuffer.append(Command{
			.paste = command
		}) catch unreachable;
	}
	fn undo(command: PasteCommand, frame: *AnimationFrame) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				command.newImage[dx + command.width*dy] = frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)];
				frame.image.imageData[@intCast(command.x + dx + (command.y + dy)*imageWidth)] = command.oldImage[dx + command.width*dy];
			}
		}
		frame.redoBuffer.append(Command{
			.paste = command
		}) catch unreachable;
	}
};

const Command = union(CommandType) {
	changePixel: ChangePixelCommand,
	changeColor: ChangeColorCommand,
	fillRect: FillRectCommand,
	paste: PasteCommand,

	fn redo(command: Command, frame: *AnimationFrame) void {
		switch(command) {
			.changePixel => command.changePixel.redo(frame),
			.changeColor => command.changeColor.redo(frame),
			.fillRect => command.fillRect.redo(frame),
			.paste => command.paste.redo(frame),
		}
	}
	fn undo(command: Command, frame: *AnimationFrame) void {
		switch(command) {
			.changePixel => command.changePixel.undo(frame),
			.changeColor => command.changeColor.undo(frame),
			.fillRect => command.fillRect.undo(frame),
			.paste => command.paste.undo(frame),
		}
	}
};

const AnimationFrame = struct {
	image: Image = .{},
	palette: [256]Color = [_]Color{.{.r = 0, .g = 0, .b = 0}} ** 256,
	undoBuffer: std.ArrayList(Command) = std.ArrayList(Command).init(allocator),
	redoBuffer: std.ArrayList(Command) = std.ArrayList(Command).init(allocator),

	pub fn copy(in: *const AnimationFrame) AnimationFrame {
		return .{
			.image = in.image,
			.palette = in.palette,
			.undoBuffer = in.undoBuffer.clone() catch unreachable,
			.redoBuffer = in.redoBuffer.clone() catch unreachable,
		};
	}

	pub fn deinit(self: *const AnimationFrame) void {
		self.undoBuffer.deinit();
		self.redoBuffer.deinit();
	}
};

var animationSequence = std.ArrayList(AnimationFrame).init(allocator);
var currentFrame: u32 = 0;
var currentColor: u8 = 0;
var selectedX: u31 = 0;
var selectedY: u31 = 0;
var selectionStartX: ?u31 = null;
var selectionStartY: ?u31 = null;
var x: i32 = 0;
var y: i32 = 0;
var scale: u31 = 10;

var keyTable: [512]i128 = [_]i128{std.math.maxInt(i128)} ** 512;

fn putInt(val: u32, buffer: []u8) []u8 {
	buffer[0] = @intCast(val & 0xff);
	buffer[1] = @intCast(val>>8 & 0xff);
	buffer[2] = @intCast(val>>16 & 0xff);
	buffer[3] = @intCast(val>>24 & 0xff);
	return buffer[0..4];
}

fn getInt(buffer: []u8) u32 {
	return @as(u32, buffer[0]) | @as(u32, buffer[1])<<8 | @as(u32, buffer[2])<<16 | @as(u32, buffer[3])<<24;
}

fn exportToPNG() !void {
	try std.fs.cwd().makePath("output");
	var outputImg: [imageWidth*imageHeight*3*outputScale*outputScale]u8 = undefined;
	for(animationSequence.items, 0..) |frame, idx| {
		var _x: u31 = 0;
		while(_x < imageWidth) : (_x += 1) {
			var _y: u31 = 0;
			while(_y < imageHeight) : (_y += 1) {
				var dx: u31 = 0;
				while(dx < outputScale) : (dx += 1) {
					var dy: u31 = 0;
					while(dy < outputScale) : (dy += 1) {
						var color = frame.image.imageData[_x + _y*imageWidth];
						var i = _x*outputScale + dx + (_y*outputScale + dy)*imageWidth*outputScale;
						outputImg[3*i] = frame.palette[color].r;
						outputImg[3*i + 1] = frame.palette[color].g;
						outputImg[3*i + 2] = frame.palette[color].b;
					}
				}
			}
		}
		var buffer: [256]u8 = [_]u8{0} ** 256;
		_ = c.stbi_write_png((try std.fmt.bufPrint(&buffer, "output/{d:0>4}.png", .{idx})).ptr, imageWidth*outputScale, imageHeight*outputScale, 3, &outputImg, imageWidth*outputScale*3);
		std.log.info("{s}", .{buffer});
	}
}

fn save() !void {
	const file = try std.fs.cwd().createFile("save.pixanim", .{});
	var buffer: [256]u8 = undefined;
	try file.writeAll(putInt(imageWidth, buffer[0..]));
	try file.writeAll(putInt(imageHeight, buffer[0..]));
	try file.writeAll(putInt(@intCast(animationSequence.items.len), buffer[0..]));
	for(animationSequence.items) |frame| {
		for(frame.palette) |color| {
			try file.writeAll(color.put(buffer[0..]));
		}
		try file.writeAll(&frame.image.imageData);
	}
	file.close();
}

fn load() !void {
	const data = try graphics.fileToString(allocator, "save.pixanim");
	std.log.info("Len{}", .{data.len});
	defer allocator.free(data);
	var offset: usize = 0;
	offset += 12; // Skip the header.
	while(offset < data.len) {
		var frame = animationSequence.addOne() catch unreachable;
		frame.* = .{};
		for(&frame.palette) |*color| {
			color.* = Color.get(data[offset..offset + 3]);
			offset += 3;
		}
		std.mem.copy(u8, frame.image.imageData[0..], data[offset..offset + imageWidth*imageHeight]);
		offset += imageWidth*imageHeight;
	}
}

fn executeCommand(key: c_int) void {
	switch(key) {
		c.GLFW_KEY_Z => {
			if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
				if(animationSequence.items[currentFrame].redoBuffer.items.len != 0) {
					animationSequence.items[currentFrame].redoBuffer.pop().redo(&animationSequence.items[currentFrame]);
				}
			} else {
				if(animationSequence.items[currentFrame].undoBuffer.items.len != 0) {
					animationSequence.items[currentFrame].undoBuffer.pop().undo(&animationSequence.items[currentFrame]);
				}
			}
		},
		c.GLFW_KEY_A => {
			if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
				animationSequence.insert(currentFrame, animationSequence.items[currentFrame].copy()) catch unreachable;
			} else {
				currentFrame -|= 1;
			}
		},
		c.GLFW_KEY_D => {
			currentFrame += 1;
			if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
				animationSequence.insert(currentFrame, animationSequence.items[currentFrame-1].copy()) catch unreachable;
			} else {
				if(currentFrame == animationSequence.items.len) {
					currentFrame -= 1;
				}
			}
		},
		else => {
			
		},
	}
}

var lastMouseX: i32 = undefined;
var lastMouseY: i32 = undefined;


fn mouseStuff(window: *c.GLFWwindow) void {
	var mouseX_float: f64 = undefined;
	var mouseY_float: f64 = undefined;
	c.glfwGetCursorPos(window, &mouseX_float, &mouseY_float);
	var mouseX: i32 = @intFromFloat(mouseX_float);
	var mouseY: i32 = @intFromFloat(mouseY_float);
	if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_RIGHT) == c.GLFW_PRESS) {
		x += mouseX - lastMouseX;
		y += mouseY - lastMouseY;
	}
	var result: i32 = @divFloor(mouseX - (x + width/2 - imageWidth*scale/2), scale);
	if(result >= 0 and result < imageWidth) selectedX = @intCast(result);
	result = @divFloor(mouseY - (y + height/2 - imageHeight*scale/2), scale);
	if(result >= 0 and result < imageHeight) selectedY = @intCast(result);
	if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
		if(selectionStartX == null) {
			selectionStartX = selectedX;
			selectionStartY = selectedY;
		}
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_LEFT) == c.GLFW_PRESS) {
			if(isKeyPressed(c.GLFW_KEY_X)) {
				PasteCommand.do(&animationSequence.items[currentFrame]);
			} else {
				FillRectCommand.do(&animationSequence.items[currentFrame]);
			}
		}
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_MIDDLE) == c.GLFW_PRESS) {
			allocator.free(copyBuffer);
			var copyX = @min(selectedX, selectionStartX.?);
			var copyY = @min(selectedY, selectionStartY.?);
			copyWidth = @max(selectedX, selectionStartX.?) - copyX;
			copyHeight = @max(selectedY, selectionStartY.?) - copyY;
			copyBuffer = allocator.alloc(u8, copyWidth*copyHeight) catch unreachable;
			var dx: u31 = 0;
			while(dx < copyWidth) : (dx += 1) {
				var dy: u31 = 0;
				while(dy < copyHeight) : (dy += 1) {
					copyBuffer[dx + copyWidth*dy] = animationSequence.items[currentFrame].image.imageData[@intCast(copyX + dx + (copyY + dy)*imageWidth)];
				}
			}
		}
	} else {
		selectionStartX = null;
		selectionStartY = null;
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_LEFT) == c.GLFW_PRESS) {
			if(isKeyPressed(c.GLFW_KEY_X)) {
				PasteCommand.do(&animationSequence.items[currentFrame]);
			} else if(mouseX > width - 300) {
				if(mouseX > width - 278 and mouseX < width - 22) {
					var color: Color = animationSequence.items[currentFrame].palette[currentColor];
					if(mouseY >= 110 and mouseY < 175) {
						color.r = @intCast(mouseX - (width - 278));
					}
					if(mouseY >= 210 and mouseY < 275) {
						color.g = @intCast(mouseX - (width - 278));
					}
					if(mouseY >= 310 and mouseY < 375) {
						color.b = @intCast(mouseX - (width - 278));
					}
					ChangeColorCommand.do(color, &animationSequence.items[currentFrame]);
				}
			} else {
				ChangePixelCommand.do(selectedX, selectedY, &animationSequence.items[currentFrame]);
				std.log.info("{}\n", .{animationSequence.items[currentFrame].undoBuffer.items.len});
			}
		}
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_MIDDLE) == c.GLFW_PRESS) {
			currentColor = animationSequence.items[currentFrame].image.imageData[@intCast(selectedX + selectedY*imageWidth)];
		}
	}

	lastMouseX = mouseX;
	lastMouseY = mouseY;
}

fn processCommands(deltaTime: i128) void {
	for(0..keyTable.len) |i| {
		keyTable[i] -= deltaTime;
		while(keyTable[i] < 0) {
			keyTable[i] += consecutiveKeyCooldown;
			executeCommand(@intCast(i));
		}
	}
}

fn isKeyPressed(key: c_int) bool {
	return keyTable[@intCast(key)] <= initialKeyCooldown;
}

fn scroll_callback(_: ?*c.GLFWwindow, xOffset: f64, yOffset: f64) callconv(.C) void {
	_ = xOffset;
	if(isKeyPressed(c.GLFW_KEY_LEFT_CONTROL) or isKeyPressed(c.GLFW_KEY_RIGHT_CONTROL)) {
		var newScale = scale + @as(i32, @intFromFloat(yOffset));
		if(newScale < 1) newScale = 1;
		x = @divFloor(x*newScale, scale);
		y = @divFloor(y*newScale, scale);
		scale = @intCast(newScale);
	} else {
		var newColor: i32 = currentColor - @as(i32, @intFromFloat(yOffset));
		if(newColor < 0) newColor = 0;
		if(newColor > 255) newColor = 255;
		currentColor = @intCast(newColor);
	}
	std.log.info("{}\n", .{yOffset});
}

fn window_size_callback(_: ?*c.GLFWwindow, newWidth: c_int, newHeight: c_int) callconv(.C) void {
	width = @intCast(newWidth);
	height = @intCast(newHeight);
}

fn key_callback(_: ?*c.GLFWwindow, key: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.C) void {
	std.log.info("{} {} {} {}\n", .{key, scancode, action, mods});
	if(key == c.GLFW_KEY_UNKNOWN) return;
	if(action == c.GLFW_PRESS) {
		keyTable[@intCast(key)] = initialKeyCooldown;
		executeCommand(key);
	}
	if(action == c.GLFW_RELEASE) {
		keyTable[@intCast(key)] = std.math.maxInt(i128);
		switch(key) {
			c.GLFW_KEY_S => {
				save() catch unreachable;
			},
			c.GLFW_KEY_E => {
				exportToPNG() catch unreachable;
			},
			c.GLFW_KEY_DELETE => {
				if(isKeyPressed(c.GLFW_KEY_LEFT_CONTROL) and isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
					// Make it hard, but possible to delete the current frame:
					if(animationSequence.items.len > 1) {
						animationSequence.orderedRemove(currentFrame).deinit();
						currentFrame = @intCast(@min(currentFrame, animationSequence.items.len-1));
					}
				}
			},
			else => {

			},
		}
	}
}

pub fn main() anyerror!void {
	defer {
		for(animationSequence.items) |*frame| {
			frame.deinit();
		}
		animationSequence.deinit();
		if(global_gpa.deinit() == .leak) {
			std.log.err("Memory leak", .{});
		}
	}
	var window: *c.GLFWwindow = undefined;

	if(c.glfwInit() == 0) {
		return error.GLFWFailed;
	}

	window = c.glfwCreateWindow(@intCast(width), @intCast(height), "pixanim", null, null) orelse return error.GLFWFailed;

	c.glfwMakeContextCurrent(window);
	c.glfwSwapInterval(0);
	if(c.gladLoadGL() == 0) {
		return error.GLADFailed;
	}

	_ = c.glfwSetWindowSizeCallback(window, window_size_callback);
	_ = c.glfwSetKeyCallback(window, key_callback);
	_ = c.glfwSetScrollCallback(window, scroll_callback);

	try graphics.init();

	var texture: c_uint = undefined;
	c.glGenTextures(1, &texture);
	c.glBindTexture(c.GL_TEXTURE_2D, texture);
	c.glTexParameteri(c.GL_TEXTURE_2D, c.GL_TEXTURE_WRAP_S, c.GL_REPEAT);
	c.glTexParameteri(c.GL_TEXTURE_2D, c.GL_TEXTURE_WRAP_T, c.GL_REPEAT);
	c.glTexParameteri(c.GL_TEXTURE_2D, c.GL_TEXTURE_MIN_FILTER, c.GL_NEAREST);
	c.glTexParameteri(c.GL_TEXTURE_2D, c.GL_TEXTURE_MAG_FILTER, c.GL_NEAREST);

	var paletteTexture: c_uint = undefined;
	c.glGenTextures(1, &paletteTexture);
	c.glBindTexture(c.GL_TEXTURE_1D, paletteTexture);
	c.glTexParameteri(c.GL_TEXTURE_1D, c.GL_TEXTURE_WRAP_S, c.GL_REPEAT);
	c.glTexParameteri(c.GL_TEXTURE_1D, c.GL_TEXTURE_MIN_FILTER, c.GL_NEAREST);
	c.glTexParameteri(c.GL_TEXTURE_1D, c.GL_TEXTURE_MAG_FILTER, c.GL_NEAREST);

	load() catch {
		_ = try animationSequence.append(.{});
	};

	var lastTime = std.time.nanoTimestamp();

	while(c.glfwWindowShouldClose(window) == 0) {
		var deltaTime = std.time.nanoTimestamp() - lastTime;
		lastTime += deltaTime;
		const glError = c.glGetError();
		if(glError != 0) {
			std.log.err("Encountered gl error {}", .{glError});
		}
		// std.log.info("{}\n", .{deltaTime});
		c.glfwPollEvents();
		processCommands(deltaTime);
		mouseStuff(window);
		c.glViewport(0, 0, @intCast(width), @intCast(height));
		c.glBindTexture(c.GL_TEXTURE_2D, texture);
		c.glTexImage2D(c.GL_TEXTURE_2D, 0, c.GL_R8, imageWidth, imageHeight, 0, c.GL_RED, c.GL_UNSIGNED_BYTE, &animationSequence.items[currentFrame].image.imageData);
		c.glBindTexture(c.GL_TEXTURE_1D, paletteTexture);
		c.glTexImage1D(c.GL_TEXTURE_1D, 0, c.GL_RGB8, 256, 0, c.GL_RGB, c.GL_UNSIGNED_BYTE, &animationSequence.items[currentFrame].palette);

		c.glActiveTexture(c.GL_TEXTURE0);
		c.glBindTexture(c.GL_TEXTURE_2D, texture);
		c.glActiveTexture(c.GL_TEXTURE1);
		c.glBindTexture(c.GL_TEXTURE_1D, paletteTexture);
		graphics.drawRect(x + width/2 - imageWidth*scale/2 - 1, y + height/2 - imageHeight*scale/2 - 1, imageWidth*scale + 2, imageHeight*scale + 2, Color{.r=255, .g=255, .b=255});
		graphics.drawImage(x + width/2 - imageWidth*scale/2, y + height/2 - imageHeight*scale/2, imageWidth*scale, imageHeight*scale);
		graphics.drawRect(
			x + width/2 - imageWidth*scale/2 + @as(i32, selectedX)*scale - 1,
			y + height/2 - imageHeight*scale/2 + @as(i32, selectedY)*scale - 1,
			2 + scale*(@as(i32, selectionStartX orelse selectedX+1) - selectedX),
			2 + scale*(@as(i32, selectionStartY orelse selectedY+1) - selectedY),
			Color{.r=255, .g=255, .b=255}
		);
		graphics.drawRect(
			x + width/2 - imageWidth*scale/2 + @as(i32, selectedX)*scale,
			y + height/2 - imageHeight*scale/2 + @as(i32, selectedY)*scale,
			scale,
			scale,
			animationSequence.items[currentFrame].palette[animationSequence.items[currentFrame].image.imageData[@intCast(selectedX + selectedY*imageWidth)]]
		);
		// Draw paste:
		if(isKeyPressed(c.GLFW_KEY_X)) {
			var dx: u31 = 0;
			while(dx < copyWidth and dx + selectedX < imageWidth) : (dx += 1) {
				var dy: u31 = 0;
				while(dy < copyHeight and dy + selectedY < imageHeight) : (dy += 1) {
					graphics.drawRect(
						x + width/2 - imageWidth*scale/2 + @as(i32, dx + selectedX)*scale,
						y + height/2 - imageHeight*scale/2 + @as(i32, dy + selectedY)*scale,
						scale,
						scale,
						animationSequence.items[currentFrame].palette[copyBuffer[dx + copyWidth*dy]]
					);
				}
			}
		}
		// Draw the palette:
		graphics.drawRect(0, 0, width, 100, Color{.r=10, .g=10, .b=10});
		graphics.drawRect(@divFloor(width, 2) - 45, 5, 90, 90, Color{.r=255, .g=255, .b=255});
		for(animationSequence.items[currentFrame].palette, 0..) |color, idx| {
			graphics.drawRect(@as(i32, @intCast(idx))*100 - @as(i32, @intCast(currentColor))*100 + @divFloor(width, 2) - 40, 10, 80, 80, color);
		}
		// Draw the color picker:
		graphics.drawRect(width - 300, 0, 300, height, Color{.r=10, .g=10, .b=10});
		if(lastMouseX > width - 300) {
			for(animationSequence.items[currentFrame].palette, 0..) |color, idx| {
				const val: u8 = @intCast(idx);
				if(color.r == val) {
					graphics.drawRect(width - 278 + val, 110, 1, 65, Color{.r=255, .g=255, .b=255});
				} else {
					graphics.drawRect(width - 278 + val, 110, 1, 40, Color{.r=val, .g=0, .b=0});
					graphics.drawRect(width - 278 + val, 155, 1, 20, Color{.r=val, .g=color.g, .b=color.b});
				}
				if(color.g == val) {
					graphics.drawRect(width - 278 + val, 210, 1, 65, Color{.r=255, .g=255, .b=255});
				} else {
					graphics.drawRect(width - 278 + val, 210, 1, 40, Color{.r=0, .g=val, .b=0});
					graphics.drawRect(width - 278 + val, 255, 1, 20, Color{.r=color.r, .g=val, .b=color.b});
				}
				if(color.b == val) {
					graphics.drawRect(width - 278 + val, 310, 1, 65, Color{.r=255, .g=255, .b=255});
				} else {
					graphics.drawRect(width - 278 + val, 310, 1, 40, Color{.r=0, .g=0, .b=val});
					graphics.drawRect(width - 278 + val, 355, 1, 20, Color{.r=color.r, .g=color.g, .b=val});
				}
			}
		}


		c.glfwSwapBuffers(window);
		c.glClear(c.GL_COLOR_BUFFER_BIT);
	}

	c.glfwTerminate();
}
