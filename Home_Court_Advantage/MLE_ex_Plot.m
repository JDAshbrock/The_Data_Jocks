head_prob = linspace(0,1,1000);
sample_prob = zeros(1,1000);
for i=1:1000
    sample_prob(i)=nchoosek(16,12)*(head_prob(i)^12)*((1-head_prob(i))^4);
end

plot(head_prob,sample_prob,'LineWidth',3)