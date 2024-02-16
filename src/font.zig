const std = @import("std");

const main = @import("main.zig");
const graphics = @import("graphics.zig");

const GlyphData = struct {
	x: u32,
	y: u32,
	width: u32,
	height: u32,
};

var glyphs: [256]GlyphData = undefined;
var atlas: graphics.DynamicImage = undefined;
var curString = std.ArrayList(u8).init(main.allocator);

pub fn init() !void {
	atlas = try graphics.DynamicImage.readFromFile(main.allocator, "font.png");
	const fontHeight = 5;
	@memset(&glyphs, .{.x = 0, .y = 0, .width = 0, .height = 0});
	var x: u32 = 0;
	var y: u32 = 0;
	for('A'..'Z'+1) |idx| {
		// Find the next column that's completely white:
		var dx: u32 = 0;
		outerBlock: while(true): (dx += 1) {
			innerBlock: {
				for(0..fontHeight) |dy| {
					if(!std.meta.eql(atlas.getRGB(x + dx, y + dy), .{.r = 255, .g = 255, .b = 255})) {
						break :innerBlock;
					}
				}
				break :outerBlock;
			}
		}
		glyphs[idx] = .{.x = x, .y = y, .width = dx, .height = fontHeight};
		x += dx + 1;
	}
	x = 0;
	y += fontHeight + 1;
	for('a'..'z'+1) |idx| {
		// Find the next column that's completely white:
		var dx: u32 = 0;
		outerBlock: while(true): (dx += 1) {
			innerBlock: {
				for(0..fontHeight) |dy| {
					if(!std.meta.eql(atlas.getRGB(x + dx, y + dy), .{.r = 255, .g = 255, .b = 255})) {
						break :innerBlock;
					}
				}
				break :outerBlock;
			}
		}
		glyphs[idx] = .{.x = x, .y = y, .width = dx, .height = fontHeight};
		x += dx + 1;
	}
	x = 0;
	y += fontHeight + 1;
	for('0'..'9'+1) |idx| {
		// Find the next column that's completely white:
		var dx: u32 = 0;
		outerBlock: while(true): (dx += 1) {
			innerBlock: {
				for(0..fontHeight) |dy| {
					if(!std.meta.eql(atlas.getRGB(x + dx, y + dy), .{.r = 255, .g = 255, .b = 255})) {
						break :innerBlock;
					}
				}
				break :outerBlock;
			}
		}
		glyphs[idx] = .{.x = x, .y = y, .width = dx, .height = fontHeight};
		x += dx + 1;
	}
	x = 0;
	y += fontHeight + 1;
	for([_]u8{' ', '.', ',', ':', ';', '-', '+', '(', ')', '[', ']', '{', '}', '/', '\\', '=', '#', '~', '<', '>', '|', '%', '\'', '"', '!', '?'}) |idx| {
		// Find the next column that's completely white:
		var dx: u32 = 0;
		outerBlock: while(true): (dx += 1) {
			innerBlock: {
				for(0..fontHeight) |dy| {
					if(!std.meta.eql(atlas.getRGB(x + dx, y + dy), .{.r = 255, .g = 255, .b = 255})) {
						break :innerBlock;
					}
				}
				break :outerBlock;
			}
		}
		glyphs[idx] = .{.x = x, .y = y, .width = dx, .height = fontHeight};
		x += dx + 1;
	}
}

pub fn deinit() void {
	atlas.deinit(main.allocator);
	curString.deinit();
}

pub fn addChar(char: u8) !void {
	try curString.append(char);
}

pub fn deleteChar() void {
	_ = curString.popOrNull();
}

pub fn renderPreview(frame: *main.AnimationFrame, x: i32, y: i32, selectedX: i32, selectedY: i32, currentColor: u8, scale: u31) void {
	var xOffset: i32 = 0;
	for(curString.items) |char| {
		const glyph = glyphs[char];
		for(0..glyph.width) |_dx| {
			const dx: u31 = @intCast(_dx);
			for(0..glyph.height) |_dy| {
				const dy: u31 = @intCast(_dy);
				if(std.meta.eql(atlas.getRGB(glyph.x + dx, glyph.y + dy), .{.r = 0, .g = 0, .b = 0})) {
					const xFull = xOffset + selectedX + @as(i32, @intCast(dx));
					const yFull = selectedY + @as(i32, @intCast(dy));
					if(xFull >= 0 and yFull >= 0 and xFull < main.imageWidth and yFull < main.imageHeight) {
						graphics.drawRect(
							x + main.width/2 - main.imageWidth*scale/2 + @as(i32, dx + xOffset + selectedX)*scale,
							y + main.height/2 - main.imageHeight*scale/2 + @as(i32, dy + selectedY)*scale,
							scale,
							scale,
							frame.palette[currentColor]
						);
					}
				}
			}
		}
		xOffset += @intCast(glyph.width + 1);
	}
}

pub fn apply(frame: *main.AnimationFrame, _x: i32, y: i32, currentColor: u8) !void { // TODO: Add logic to undo this.
	var x: i32 = _x;
	for(curString.items) |char| {
		const glyph = glyphs[char];
		for(0..glyph.width) |dx| {
			for(0..glyph.height) |dy| {
				if(std.meta.eql(atlas.getRGB(glyph.x + dx, glyph.y + dy), .{.r = 0, .g = 0, .b = 0})) {
					const xFull = x + @as(i32, @intCast(dx));
					const yFull = y + @as(i32, @intCast(dy));
					if(xFull >= 0 and yFull >= 0 and xFull < main.imageWidth and yFull < main.imageHeight) {
						frame.image.set(@intCast(x + @as(i32, @intCast(dx))), @intCast(y + @as(i32, @intCast(dy))), currentColor);
					}
				}
			}
		}
		x += @intCast(glyph.width + 1);
	}
}

pub fn clearBuffer() void {
	curString.clearRetainingCapacity();
}