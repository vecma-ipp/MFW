import fileinput
import os, sys

def replace(f, s1, r1, s2, r2):
    for l in fileinput.input(f, inplace=1):
        if s1 in l:
            l = l.replace(s1, r1)
        if s2 in l:
            l = l.replace(s2, r2)
        sys.stdout.write(l)

s1 = 'SBATCH --nodes=1'
s2 = 'SBATCH --job-name=SEQ'
n = [1, 2, 4, 8]
for i in n:
    fi = 'run_'+str(i)+'.sh'
    os.system('cp run.sh '+fi)
    r1 = 'SBATCH --nodes='+str(i)
    r2 = 'SBATCH --job-name=TC3_'+str(i)
    replace(fi, s1, r1, s2, r2)
    os.system('sbatch '+fi)
