% Multivariate regression in Matlab
%
% Read in monthly flows for 04118500
% save('multreg.mat');
load('multreg.mat');
% Beg estimation period index
ndx1160beg = find(Year1160==1952 & Month1160 == 2);
ndx1165beg = find(Year1165==1952 & Month1165 == 2);
ndx1180beg = find(Year1180==1952 & Month1180 == 2);
ndx1185beg = find(Year1185==1952 & Month1185 == 2);
% End estimation period index
ndx1160end = find(Year1160==1982 & Month1160 == 3);
ndx1165end = find(Year1165==1982 & Month1165 == 3);
ndx1180end = find(Year1180==1982 & Month1180 == 3);
ndx1185end = find(Year1185==1982 & Month1185 == 3);
% Estimation flows
estLogQ1160 = log10(Flow1160(ndx1160beg:ndx1160end));
estLogQ1165 = log10(Flow1165(ndx1165beg:ndx1165end));
estLogQ1180 = log10(Flow1180(ndx1180beg:ndx1180end));
estLogQ1185 = log10(Flow1185(ndx1185beg:ndx1185end));
%
[beta,Sigma] = mvregress([ones(size(estLogQ1160)),estLogQ1160],...
    [estLogQ1165,estLogQ1180,estLogQ1185]);


