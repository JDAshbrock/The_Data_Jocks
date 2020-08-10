close all
p1 = 0.320;
p2 = 0.320;
z=1.96;

season_length = 225:1:730;
len = length(season_length);

p1_upper=zeros(1,len);
p1_lower =zeros(1,len);
p2_upper=zeros(1,len);
p2_lower=zeros(1,len);


for i=1:len
   p1_upper(i) = p1*season_length(i)+z*sqrt(season_length(i)*p1*(1-p1));
   p1_lower(i) = p1*season_length(i)-z*sqrt(season_length(i)*p1*(1-p1));
   p2_upper(i) = p2*season_length(i)+z*sqrt(season_length(i)*p2*(1-p2));
   p2_lower(i) = p2*season_length(i)-z*sqrt(season_length(i)*p2*(1-p2));
end

plot(season_length,p1_upper./season_length,'b')
hold on
plot(season_length,p1_lower./season_length,'b')
plot(season_length,p2_upper./season_length,'r')
plot(season_length,p2_lower./season_length,'r')
plot(season_length,p1+zeros(1,len),'b')
plot(season_length,p2+zeros(1,len),'r')