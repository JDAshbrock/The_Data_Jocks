walk_runs =[0,5138,1244,1583,753,735,325,133,55,22,12];
regular_runs = [3516,1870,1469,969,1397,422,220,84,31,13,9];

for i=2:length(walk_runs)
    walk_runs(i)=walk_runs(i)+walk_runs(i-1);
    regular_runs(i)=regular_runs(i)+regular_runs(i-1);
end
walk_runs=walk_runs/10000;
regular_runs=regular_runs/10000;
scatter(1:(length(walk_runs)),walk_runs,'LineWidth',2)
hold on
scatter(1:(length(walk_runs)),regular_runs,'LineWidth',2)