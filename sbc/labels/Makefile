# build tools & options
.DEFAULT_GOAL := labels

labels: custom.yaml
	~/opt/chiplabel/chip_label.py -v -i . -o . --fontSize 1.9 -a

clean:
	rm *.png
