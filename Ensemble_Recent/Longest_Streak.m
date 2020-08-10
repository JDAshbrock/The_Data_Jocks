simulations = 1000;
n_flips = 82;
win_pct =0.65;

longest_streaks = zeros(1,simulations);
for i=1:simulations
    flip = rand;
       if flip <win_pct
           flip =0;
       else
           flip=1;
       end
     prev_flip = flip;
     cur_streak=1;
     longest=1;
   for j=2:n_flips
       %flip a coin, 0=heads, 1=tails
       flip = rand;
       if flip <win_pct
           flip =0;
       else
           flip=1;
       end
       %if flip is same sa previous, increase current streak length
       if flip == prev_flip
           cur_streak = cur_streak+1;
       else
           cur_streak=1;
       end
       if cur_streak >longest
           longest=cur_streak;
       end
   end%end simulation
   longest_streaks(i)=longest;
end

uniques = unique(longest_streaks);
C=hist(longest_streaks,uniques);
bar(uniques,C)