const std = @import("std");
const builtin = @import("builtin");

const graphics = @import("graphics.zig");

pub const c = @cImport ({
	@cInclude("glad/glad.h");
	@cInclude("GLFW/glfw3.h");
});

const initialKeyCooldown: i128 = 300_000_000;
const consecutiveKeyCooldown: i128 = 50_000_000;

pub var width: u31 = 1920;
pub var height: u31 = 1080;
const imageWidth: u31 = 192;
const imageHeight: u31 = 108;

const Color = graphics.Color;

const allocator = std.heap.page_allocator;
var palette: [256]Color = undefined;

const Image = extern struct {
	imageData: [imageWidth*imageHeight]u8 = [_]u8{0}**(imageWidth * imageHeight),
};

var copyBuffer: []u8 = "";
var copyWidth: u31 = 0;
var copyHeight: u31 = 0;

var undoBuffer = std.ArrayList(Command).init(allocator);
var redoBuffer = std.ArrayList(Command).init(allocator);

const CommandType = enum {
	changePixel,
	changeColor,
	fillRect,
	paste,
};

const ChangePixelCommand = struct {
	x: u32,
	y: u32,
	z: u32,
	oldColor: u8,
	newColor: u8,
	fn do(posX: u31, posY: u31) void {
		const command = ChangePixelCommand{
			.x = posX,
			.y = posY,
			.z = currentImage,
			.oldColor = animationSequence.items[currentImage].imageData[@intCast(usize, posX + posY*imageWidth)],
			.newColor = currentColor
		};
		if(command.oldColor == command.newColor) return;
		command.redo();
	}
	fn redo(command: ChangePixelCommand) void {
		animationSequence.items[command.z].imageData[@intCast(usize, command.x + command.y*imageWidth)] = command.newColor;
		undoBuffer.append(Command{
			.changePixel = command
		}) catch unreachable;
	}
	fn undo(command: ChangePixelCommand) void {
		animationSequence.items[command.z].imageData[@intCast(usize, command.x + command.y*imageWidth)] = command.oldColor;
		redoBuffer.append(Command{
			.changePixel = command
		}) catch unreachable;
	}
};

const ChangeColorCommand = struct {
	idx: u8,
	oldColor: Color,
	newColor: Color,
	fn do(newColor: Color) void {
		const command = ChangeColorCommand{
			.idx = currentColor,
			.oldColor = palette[currentColor],
			.newColor = newColor
		};
		if(std.meta.eql(command.oldColor, command.newColor)) return;
		command.redo();
	}
	fn redo(command: ChangeColorCommand) void {
		palette[command.idx] = command.newColor;
		if(undoBuffer.items.len != 0 and undoBuffer.items[undoBuffer.items.len - 1] == .changeColor and undoBuffer.items[undoBuffer.items.len - 1].changeColor.idx == command.idx) {
			undoBuffer.items[undoBuffer.items.len - 1].changeColor.newColor = command.newColor;
		} else {
			undoBuffer.append(Command{
				.changeColor = command
			}) catch unreachable;
		}
	}
	fn undo(command: ChangeColorCommand) void {
		std.log.info("{}, {}", .{command.oldColor.r, palette[command.idx].r});
		palette[command.idx] = command.oldColor;
		redoBuffer.append(Command{
			.changeColor = command
		}) catch unreachable;
	}
};

const FillRectCommand = struct {
	x: u32,
	y: u32,
	width: u32,
	height: u32,
	z: u32,
	oldImage: []u8,
	newColor: u8,
	fn do() void {
		var command = FillRectCommand{
			.x = @minimum(selectionStartX.?,  selectedX),
			.y = @minimum(selectionStartY.?,  selectedY),
			.width = @maximum(selectionStartX.?, selectedX) - @minimum(selectionStartX.?,  selectedX),
			.height = @maximum(selectionStartY.?, selectedY) - @minimum(selectionStartY.?,  selectedY),
			.z = currentImage,
			.oldImage = undefined,
			.newColor = currentColor
		};
		var isRepeat = true;
		var dx: u31 = 0;
		outer: while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				if(command.newColor != animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)]) {
					isRepeat = false;
					break :outer;
				}
			}
		}
		if(isRepeat) return;
		command.oldImage = allocator.alloc(u8, command.width*command.height) catch unreachable;
		command.redo();
	}
	fn redo(command: FillRectCommand) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				command.oldImage[dx + command.width*dy] = animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)];
				animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)] = command.newColor;
			}
		}
		undoBuffer.append(Command{
			.fillRect = command
		}) catch unreachable;
	}
	fn undo(command: FillRectCommand) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)] = command.oldImage[dx + command.width*dy];
			}
		}
		redoBuffer.append(Command{
			.fillRect = command
		}) catch unreachable;
	}
};

const PasteCommand = struct {
	x: u32,
	y: u32,
	width: u32,
	height: u32,
	z: u32,
	oldImage: []u8,
	newImage: []u8,
	fn do() void {
		var command = PasteCommand{
			.x = selectedX,
			.y = selectedY,
			.width = @minimum(copyWidth, imageWidth - selectedX),
			.height = @minimum(copyHeight, imageHeight - selectedY),
			.z = currentImage,
			.oldImage = undefined,
			.newImage = undefined
		};
		var isRepeat = true;
		var dx: u31 = 0;
		outer: while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				if(copyBuffer[dx + dy*copyWidth] != animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)]) {
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
		command.redo();
	}
	fn redo(command: PasteCommand) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				command.oldImage[dx + command.width*dy] = animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)];
				animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)] = command.newImage[dx + command.width*dy];
			}
		}
		undoBuffer.append(Command{
			.paste = command
		}) catch unreachable;
	}
	fn undo(command: PasteCommand) void {
		var dx: u31 = 0;
		while(dx < command.width) : (dx += 1) {
			var dy: u31 = 0;
			while(dy < command.height) : (dy += 1) {
				command.newImage[dx + command.width*dy] = animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)];
				animationSequence.items[command.z].imageData[@intCast(usize, command.x + dx + (command.y + dy)*imageWidth)] = command.oldImage[dx + command.width*dy];
			}
		}
		redoBuffer.append(Command{
			.paste = command
		}) catch unreachable;
	}
};

const Command = union(CommandType) {
	changePixel: ChangePixelCommand,
	changeColor: ChangeColorCommand,
	fillRect: FillRectCommand,
	paste: PasteCommand,

	fn redo(command: Command) void {
		switch(command) {
			.changePixel => command.changePixel.redo(),
			.changeColor => command.changeColor.redo(),
			.fillRect => command.fillRect.redo(),
			.paste => command.paste.redo(),
		}
	}
	fn undo(command: Command) void {
		switch(command) {
			.changePixel => command.changePixel.undo(),
			.changeColor => command.changeColor.undo(),
			.fillRect => command.fillRect.undo(),
			.paste => command.paste.undo(),
		}
	}
};

var animationSequence = std.ArrayList(Image).init(allocator);
var currentImage: u32 = 0;
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
	buffer[0] = @intCast(u8, val & 0xff);
	buffer[1] = @intCast(u8, val>>8 & 0xff);
	buffer[2] = @intCast(u8, val>>16 & 0xff);
	buffer[3] = @intCast(u8, val>>24 & 0xff);
	return buffer[0..4];
}

fn getInt(buffer: []u8) u32 {
	return @intCast(u32, buffer[0]) | @intCast(u32, buffer[1])<<8 | @intCast(u32, buffer[2])<<16 | @intCast(u32, buffer[3])<<24;
}

fn save() !void {
	const file = try std.fs.cwd().createFile("save.pixanim", .{});
	var buffer: [256]u8 = undefined;
	for(palette) |color| {
		try file.writeAll(color.put(buffer[0..]));
	}
	try file.writeAll(putInt(imageWidth, buffer[0..]));
	try file.writeAll(putInt(imageHeight, buffer[0..]));
	try file.writeAll(putInt(@intCast(u32, animationSequence.items.len), buffer[0..]));
	for(animationSequence.items) |image| {
		try file.writeAll(&image.imageData);
	}
	file.close();
}

fn load() !void {
	const data = try graphics.fileToString(allocator, "save.pixanim");
	std.log.info("Len{}", .{data.len});
	defer allocator.free(data);
	var offset: usize = 0;
	for(palette) |*color| {
		color.* = Color.get(data[offset..offset + 3]);
		offset += 3;
	}
	offset += 12; // Skip the header.
	while(offset < data.len) : (offset += imageWidth*imageHeight) {
		var image = animationSequence.addOne() catch unreachable;
		std.mem.copy(u8, image.imageData[0..], data[offset..offset + imageWidth*imageHeight]);
	}
}

fn executeCommand(key: c_int) void {
	switch(key) {
		c.GLFW_KEY_S => {
			save() catch unreachable;
		},
		c.GLFW_KEY_Z => {
			if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
				if(redoBuffer.items.len != 0) {
					redoBuffer.pop().redo();
				}
			} else {
				if(undoBuffer.items.len != 0) {
					undoBuffer.pop().undo();
				}
			}
		},
		c.GLFW_KEY_A => {
			if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
				animationSequence.insert(currentImage, animationSequence.items[currentImage]) catch unreachable;
			} else {
				currentImage -|= 1;
			}
		},
		c.GLFW_KEY_D => {
			currentImage += 1;
			if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
				animationSequence.insert(currentImage, animationSequence.items[currentImage-1]) catch unreachable;
			} else {
				if(currentImage == animationSequence.items.len) {
					currentImage -= 1;
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
	var mouseX: i32 = @floatToInt(i32, mouseX_float);
	var mouseY: i32 = @floatToInt(i32, mouseY_float);
	if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_RIGHT) == c.GLFW_PRESS) {
		x += mouseX - lastMouseX;
		y += mouseY - lastMouseY;
	}
	var result: i32 = @divFloor(mouseX - (x + width/2 - imageWidth*scale/2), scale);
	if(result >= 0 and result < imageWidth) selectedX = @intCast(u31, result);
	result = @divFloor(mouseY - (y + height/2 - imageHeight*scale/2), scale);
	if(result >= 0 and result < imageHeight) selectedY = @intCast(u31, result);
	if(isKeyPressed(c.GLFW_KEY_LEFT_SHIFT) or isKeyPressed(c.GLFW_KEY_RIGHT_SHIFT)) {
		if(selectionStartX == null) {
			selectionStartX = selectedX;
			selectionStartY = selectedY;
		}
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_LEFT) == c.GLFW_PRESS) {
			if(isKeyPressed(c.GLFW_KEY_X)) {
				PasteCommand.do();
			} else {
				FillRectCommand.do();
			}
		}
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_MIDDLE) == c.GLFW_PRESS) {
			allocator.free(copyBuffer);
			var copyX = @minimum(selectedX, selectionStartX.?);
			var copyY = @minimum(selectedY, selectionStartY.?);
			copyWidth = @maximum(selectedX, selectionStartX.?) - copyX;
			copyHeight = @maximum(selectedY, selectionStartY.?) - copyY;
			copyBuffer = allocator.alloc(u8, copyWidth*copyHeight) catch unreachable;
			var dx: u31 = 0;
			while(dx < copyWidth) : (dx += 1) {
				var dy: u31 = 0;
				while(dy < copyHeight) : (dy += 1) {
					copyBuffer[dx + copyWidth*dy] = animationSequence.items[currentImage].imageData[@intCast(usize, copyX + dx + (copyY + dy)*imageWidth)];
				}
			}
		}
	} else {
		selectionStartX = null;
		selectionStartY = null;
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_LEFT) == c.GLFW_PRESS) {
			if(isKeyPressed(c.GLFW_KEY_X)) {
				PasteCommand.do();
			} else if(mouseX > width - 300) {
				if(mouseX > width - 278 and mouseX < width - 22) {
					var color: Color = palette[currentColor];
					if(mouseY >= 110 and mouseY < 175) {
						color.r = @intCast(u8, mouseX - (width - 278));
					}
					if(mouseY >= 210 and mouseY < 275) {
						color.g = @intCast(u8, mouseX - (width - 278));
					}
					if(mouseY >= 310 and mouseY < 375) {
						color.b = @intCast(u8, mouseX - (width - 278));
					}
					ChangeColorCommand.do(color);
				}
			} else {
				ChangePixelCommand.do(selectedX, selectedY);
				std.log.info("{}\n", .{undoBuffer.items.len});
			}
		}
		if(c.glfwGetMouseButton(window, c.GLFW_MOUSE_BUTTON_MIDDLE) == c.GLFW_PRESS) {
			currentColor = animationSequence.items[currentImage].imageData[@intCast(usize, selectedX + selectedY*imageWidth)];
		}
	}

	lastMouseX = mouseX;
	lastMouseY = mouseY;
}

fn processCommands(deltaTime: i128) void {
	for(keyTable) |_, i| {
		keyTable[i] -= deltaTime;
		while(keyTable[i] < 0) {
			keyTable[i] += consecutiveKeyCooldown;
			executeCommand(@intCast(c_int, i));
		}
	}
}

fn isKeyPressed(key: c_int) bool {
	return keyTable[@intCast(usize, key)] <= initialKeyCooldown;
}

fn scroll_callback(_: ?*c.GLFWwindow, xOffset: f64, yOffset: f64) callconv(.C) void {
	_ = xOffset;
	if(isKeyPressed(c.GLFW_KEY_LEFT_CONTROL) or isKeyPressed(c.GLFW_KEY_RIGHT_CONTROL)) {
		var newScale = scale + @floatToInt(i32, yOffset);
		if(newScale < 1) newScale = 1;
		x = @divFloor(x*newScale, scale);
		y = @divFloor(y*newScale, scale);
		scale = @intCast(u31, newScale);
	} else {
		var newColor: i32 = currentColor - @floatToInt(i32, yOffset);
		if(newColor < 0) newColor = 0;
		if(newColor > 255) newColor = 255;
		currentColor = @intCast(u8, newColor);
	}
	std.log.info("{}\n", .{yOffset});
}

fn window_size_callback(_: ?*c.GLFWwindow, newWidth: c_int, newHeight: c_int) callconv(.C) void {
	width = @intCast(u31, newWidth);
	height = @intCast(u31, newHeight);
}

fn key_callback(_: ?*c.GLFWwindow, key: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.C) void {
	std.log.info("{} {} {} {}\n", .{key, scancode, action, mods});
	if(key == c.GLFW_KEY_UNKNOWN) return;
	if(action == c.GLFW_PRESS) {
		keyTable[@intCast(usize, key)] = initialKeyCooldown;
		executeCommand(key);
	}
	if(action == c.GLFW_RELEASE) {
		keyTable[@intCast(usize, key)] = std.math.maxInt(i128);
	}
}

pub fn main() anyerror!void {
	var window: *c.GLFWwindow = undefined;

	if(c.glfwInit() == 0) {
		return error.GLFWFailed;
	}

	_ = window;

	window = c.glfwCreateWindow(@intCast(c_int, width), @intCast(c_int, height), "pixanim", null, null) orelse return error.GLFWFailed;

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
		_ = try animationSequence.append(Image{});
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
		c.glViewport(0, 0, @intCast(c_int, width), @intCast(c_int, height));
		c.glBindTexture(c.GL_TEXTURE_2D, texture);
		c.glTexImage2D(c.GL_TEXTURE_2D, 0, c.GL_R8, imageWidth, imageHeight, 0, c.GL_RED, c.GL_UNSIGNED_BYTE, &animationSequence.items[currentImage].imageData);
		c.glBindTexture(c.GL_TEXTURE_1D, paletteTexture);
		c.glTexImage1D(c.GL_TEXTURE_1D, 0, c.GL_RGB8, 256, 0, c.GL_RGB, c.GL_UNSIGNED_BYTE, &palette);

		c.glActiveTexture(c.GL_TEXTURE0);
		c.glBindTexture(c.GL_TEXTURE_2D, texture);
		c.glActiveTexture(c.GL_TEXTURE1);
		c.glBindTexture(c.GL_TEXTURE_1D, paletteTexture);
		graphics.drawRect(x + width/2 - imageWidth*scale/2 - 1, y + height/2 - imageHeight*scale/2 - 1, imageWidth*scale + 2, imageHeight*scale + 2, Color{.r=255, .g=255, .b=255});
		graphics.drawImage(x + width/2 - imageWidth*scale/2, y + height/2 - imageHeight*scale/2, imageWidth*scale, imageHeight*scale);
		graphics.drawRect(
			x + width/2 - imageWidth*scale/2 + @intCast(i32, selectedX)*scale - 1,
			y + height/2 - imageHeight*scale/2 + @intCast(i32, selectedY)*scale - 1,
			2 + scale*(@intCast(i32, selectionStartX orelse selectedX+1) - selectedX),
			2 + scale*(@intCast(i32, selectionStartY orelse selectedY+1) - selectedY),
			Color{.r=255, .g=255, .b=255}
		);
		graphics.drawRect(
			x + width/2 - imageWidth*scale/2 + @intCast(i32, selectedX)*scale,
			y + height/2 - imageHeight*scale/2 + @intCast(i32, selectedY)*scale,
			scale,
			scale,
			palette[animationSequence.items[currentImage].imageData[@intCast(usize, selectedX + selectedY*imageWidth)]]
		);
		// Draw paste:
		if(isKeyPressed(c.GLFW_KEY_X)) {
			var dx: u31 = 0;
			while(dx < copyWidth and dx + selectedX < imageWidth) : (dx += 1) {
				var dy: u31 = 0;
				while(dy < copyHeight and dy + selectedY < imageHeight) : (dy += 1) {
					graphics.drawRect(
						x + width/2 - imageWidth*scale/2 + @intCast(i32, dx + selectedX)*scale,
						y + height/2 - imageHeight*scale/2 + @intCast(i32, dy + selectedY)*scale,
						scale,
						scale,
						palette[copyBuffer[dx + copyWidth*dy]]
					);
				}
			}
		}
		// Draw the palette:
		graphics.drawRect(0, 0, width, 100, Color{.r=10, .g=10, .b=10});
		graphics.drawRect(@divFloor(width, 2) - 45, 5, 90, 90, Color{.r=255, .g=255, .b=255});
		for(palette) |color, idx| {
			graphics.drawRect(@intCast(i32, idx)*100 - @intCast(i32, currentColor)*100 + @divFloor(width, 2) - 40, 10, 80, 80, color);
		}
		// Draw the color picker:
		graphics.drawRect(width - 300, 0, 300, height, Color{.r=10, .g=10, .b=10});
		if(lastMouseX > width - 300) {
			for(palette) |_, idx| {
				const val = @intCast(u8, idx);
				if(palette[currentColor].r == val) {
					graphics.drawRect(width - 278 + val, 110, 1, 65, Color{.r=255, .g=255, .b=255});
				} else {
					graphics.drawRect(width - 278 + val, 110, 1, 40, Color{.r=val, .g=0, .b=0});
					graphics.drawRect(width - 278 + val, 155, 1, 20, Color{.r=val, .g=palette[currentColor].g, .b=palette[currentColor].b});
				}
				if(palette[currentColor].g == val) {
					graphics.drawRect(width - 278 + val, 210, 1, 65, Color{.r=255, .g=255, .b=255});
				} else {
					graphics.drawRect(width - 278 + val, 210, 1, 40, Color{.r=0, .g=val, .b=0});
					graphics.drawRect(width - 278 + val, 255, 1, 20, Color{.r=palette[currentColor].r, .g=val, .b=palette[currentColor].b});
				}
				if(palette[currentColor].b == val) {
					graphics.drawRect(width - 278 + val, 310, 1, 65, Color{.r=255, .g=255, .b=255});
				} else {
					graphics.drawRect(width - 278 + val, 310, 1, 40, Color{.r=0, .g=0, .b=val});
					graphics.drawRect(width - 278 + val, 355, 1, 20, Color{.r=palette[currentColor].r, .g=palette[currentColor].g, .b=val});
				}
			}
		}


		c.glfwSwapBuffers(window);
		c.glClear(c.GL_COLOR_BUFFER_BIT);
	}

	c.glfwTerminate();
}
