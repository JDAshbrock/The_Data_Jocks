pt = 0.95;
pf = 0.05;

pos_rate =linspace(.05,.95,1000);
corona_rate = zeros(1000,1);
variance = zeros(1000,1);
lower_estimate =zeros(1000,1);
upper_estimate = zeros(1000,1);
n=100;

for i=1:1000
    corona_rate(i) = (pos_rate(i)-pf)/(pt-pf);
    variance(i) = (pos_rate(i)*(1-pos_rate(i)))/(n*((pf-pt)^2));
    upper_estimate(i)=corona_rate(i)+1.96*sqrt(variance(i));
    lower_estimate(i)=corona_rate(i)-1.96*sqrt(variance(i));
end

plot(pos_rate,corona_rate,'r','LineWIdth',3)
hold on
plot(pos_rate,upper_estimate,'b--');
plot(pos_rate,lower_estimate,'b-.');
legend