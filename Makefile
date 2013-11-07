CODE_DIR = code/R
TEX_DIR = writeup

all:
	$(MAKE) -C $(CODE_DIR)
	$(MAKE) -C $(TEX_DIR)

clean:
	$(MAKE) -C $(TEX_DIR) clean