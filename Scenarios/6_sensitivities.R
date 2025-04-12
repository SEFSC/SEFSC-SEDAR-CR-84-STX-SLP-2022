#_s1 higher CV on catch
#_s2 higher CV on growth young
#_s3 higher CV on growth
#_s4 higher age and lower m
#_s5 lower age and higher m
#_s6 fecundity

# It is important to code this because running 6 sensitivities on 5 models would be too much to do manually and this set us up for the other islands.

# Now that time has been invested in automating the diagnostics and repair code, I have the tools and knowledge to write this pretty quickly. The strategy is to break down the runs to understand what files need to be changed. for example, max age and CV on catch is in the data file, while CV on growth and m is in the control file. Hermaphroditism as fecundity in a one sex model is the most complex and will need to be done manually.

# I just realized I need to rerun all models because I used the max age of 19. Not sure why I did that, but luckily I have the time today to let it rip. By that I mean use the automation to repair and rerun. It is actually a good mistake, because now I can also better document the process. I am quite excited to share this repo with everyone just to show my approach and way of solving the issues I have faced. I need to figure out what is the best way to start sharing this. I think a new repo makes sense, but I don't have the time or skills to really set it up as an easy way of storing code there that executes here. A interm solution can be a markdown issue in this same repo. This documentation will help me a ton when I go to run the code and move it into other repos for the other S84 islands.