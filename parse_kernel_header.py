#!/usr/bin/env python3

import sys
import struct
import pprint

KERNEL_SETUP_HEADER_OFFSET = 0x01f1
KERNEL_SETUP_HEADER_LEN = 0x007b

def main():
	kernel_image_path = sys.argv[1]

	with open(kernel_image_path, "rb") as f:
		f.seek(KERNEL_SETUP_HEADER_OFFSET)
		header_data = f.read(KERNEL_SETUP_HEADER_LEN)

	header = dict()
	header["setup_sects"], \
	header["root_flags"], \
	header["syssize"], \
	header["ram_size"], \
	header["vid_mode"], \
	header["root_dev"], \
	header["boot_flag"], \
	header["jump"], \
	header["header"], \
	header["version"], \
	header["realmode_swtch"], \
	header["start_sys_seg"], \
	header["kernel_version"], \
	header["type_of_loader"], \
	header["loadflags"], \
	header["setup_move_size"], \
	header["code32_start"], \
	header["ramdisk_image"], \
	header["ramdisk_size"], \
	header["bootsect_kludge"], \
	header["heap_end_ptr"], \
	header["ext_loader_ver"], \
	header["ext_loader_type"], \
	header["cmd_line_ptr"], \
	header["initrd_addr_max"], \
	header["kernel_alignment"], \
	header["relocatable_kernel"], \
	header["min_alignment"], \
	header["xloadflags"], \
	header["cmdline_size"], \
	header["hardware_subarch"], \
	header["hardware_subarch_data"], \
	header["payload_offset"], \
	header["payload_length"], \
	header["setup_data"], \
	header["pref_address"], \
	header["init_size"], \
	header["handover_offset"], \
	header["kernel_info_offset"] = struct.unpack("<B H I H H H H H 4s H I H H B B H I I I I H B B I I I B B H I I Q I I Q Q I I I", header_data)

	pprint.pp(header)

if __name__ == "__main__":
	main()
