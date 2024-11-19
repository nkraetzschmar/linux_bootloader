.SILENT:
.PHONY: clean test run_kernel

disk: mbr.bin bzImage
	echo "combining $^ -> $@"
	dd if=/dev/zero of=$@ bs=512 count=262144 2> /dev/null
	dd if=$(word 1,$^) of=$@ bs=512 count=1 conv=notrunc 2> /dev/null
	dd if=$(word 2,$^) of=$@ bs=512 seek=33 conv=notrunc 2> /dev/null

mbr.bin: mbr.asm
	echo "building $^ -> $@"
	nasm -f bin -o $@ $<
	hexdump -vC $@

clean:
	rm -f mbr.bin disk

test: disk
	echo "running $< in qemu"
	qemu-system-x86_64 -machine pc -cpu qemu64 -accel tcg -m 1024 -nodefaults -no-reboot -nographic -serial stdio -drive file=$<,format=raw | sed 's/\x1b[\[0-9;?]*[a-zA-Z]//g'
