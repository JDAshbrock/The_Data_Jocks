close all
p=0.320;
simulations = 100000;
games = 60;
AB_per = 4.5;
z1=1.96;
z2= 2.576;


total_AB=games*AB_per;
hits = binornd(total_AB,p, simulations,1);
hits = hits/total_AB;

histogram(hits, 'Normalization', 'probability')
x_high1=zeros(1,100)+p+z1*sqrt((p*(1-p))/total_AB);
x_low1 =zeros(1,100)+p-z1*sqrt((p*(1-p))/total_AB);
x_high2=zeros(1,100)+p+z2*sqrt((p*(1-p))/total_AB);
x_low2=zeros(1,100)+p-z2*sqrt((p*(1-p))/total_AB);

y=linspace(0,0.06,100);
hold on
plot(x_high1,y,'r','LineWidth',2)
plot(x_low1,y,'r','LineWidth',2)
plot(x_high2,y,'g','LineWidth',2)
plot(x_low2,y,'g','LineWidth',2)