const std = @import("std");

pub fn build(b: *std.Build) !void {
	// Standard target options allows the person running `zig build` to choose
	// what target to build for. Here we do not override the defaults, which
	// means any target is allowed, and the default is native. Other options
	// for restricting supported target set are available.
	const target = b.standardTargetOptions(.{});
	const t = target.result;

	// Standard release options allow the person running `zig build` to select
	// between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
	const optimize = b.standardOptimizeOption(.{});
	const exe = b.addExecutable(.{
		.name = "Cubyzig",
		.root_source_file = .{.path = "src/main.zig"},
		.target = target,
		.optimize = optimize,
	});

	exe.addIncludePath(.{.path = "include"});
	exe.addCSourceFiles(.{.files = &[_][]const u8{"lib/src/glad.c", "lib/src/stb_image_write.c", "lib/src/stb_image.c"}, .flags = &[_][]const u8{"-g", "-O3"}});
	switch (t.os.tag) {
		.linux => {
			exe.linkSystemLibrary("GL");
			exe.linkSystemLibrary("glfw");
		},
		else => {
			std.log.err("Unsupported target: {}\n", .{ t.os.tag });
			return;
		}
	}
	exe.linkLibC();
	b.installArtifact(exe);

	const run_cmd = b.addRunArtifact(exe);
	run_cmd.step.dependOn(b.getInstallStep());
	if (b.args) |args| {
		run_cmd.addArgs(args);
	}

	const run_step = b.step("run", "Run the app");
	run_step.dependOn(&run_cmd.step);
}
