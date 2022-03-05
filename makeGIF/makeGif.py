from PIL import Image
import glob

#dirname = '../result/isoclines'
#dirname = '../result/vectorField3d'
dirname = '../result_patch'
filename = dirname + '/*.png'
files = sorted(glob.glob(filename))
images = list(map(lambda file: Image.open(file), files))

gifname = dirname + '.gif'
images[0].save(gifname, save_all=True, append_images=images[1:], duration=300, loop=0)
