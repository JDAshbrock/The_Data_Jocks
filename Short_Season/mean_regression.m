ba = .3;
max_abs = 1500;
at_bats = 50:1:1500;
num_sims =6;
running_averages = zeros(length(at_bats),num_sims);

for i=1:num_sims
    data = binornd(1,ba,max_abs,1);
    for j=50:max_abs
        running_averages(j-49,i)=mean(data(1:j));
    end
end

plot(at_bats,running_averages(:,1))
hold on
plot(at_bats,running_averages(:,2))
plot(at_bats,running_averages(:,3))
plot(at_bats,running_averages(:,4))
plot(at_bats,running_averages(:,5))
plot(at_bats,running_averages(:,6))
plot(270+zeros(100,1),linspace(.22,.38),'bl','LineWidth',3)