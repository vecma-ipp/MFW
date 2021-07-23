import imageio
from PIL import Image
from pdf2image import convert_from_path

images = []
filenames = []
#fp_in = 'surr_gem0_'
#fp_out = 'gpr_gem0_te.gif'

fp_in = 'Te_surr2d_gem0_'
fp_out = 'gpr_gem0_tegradte.gif'

#for i in range(4,20):
#	filenames.append(fp_in + str(i) + '.pdf')
	
#for filename in filenames:
#    images = convert_from_path(filename)
	
for i in range(8,20):
	filenames.append(fp_in + str(i) + '.png')

with imageio.get_writer(fp_out, mode='I', duration = 0.6) as writer:
	for filename in filenames:
		#images.append(imageio.imread(filename))
		writer.append_data(imageio.imread(filename))
	
#imageio.mimsave(fp_out, images)
