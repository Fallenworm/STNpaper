clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
load("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat.mat");
% 2-->LFPnodes
% 3-->EEGnodes
% 4-->OCIR
% 5-->BDI
% 6-->AES
% 7-->AES(12PD)
% 8-->BAI
% 9-->QUIP
% 10-13 --> AES(27/28 cutoff); BDI(6/7 cutoff); BAI(12/13 cutoff); QUIP(0/1 cutoff)
% 14-->on UPDRS
% 15-->off UPDRS
% 16-->moca
% 17-->LEED
% 18-23-->(Lack of) premeditation;(Negative) Urgency;Sensation
% seeking;(Lack of) preseverance;Positive Urgency; total
% 24-26--> cognitive/behavioral/emotional AES
% 27-29--> XYZ MNI coordinates
% 30-->Frequency spectrum
% 31-33-->STN EEG power coherence
% 34-->EEG(1-12Hz/1Hz) to STN (13-35Hz/1Hz) PAC
% 35-->EEG(1-40Hz/1Hz) to STN (40-90Hz/4Hz) PAC
% 36-->STN(1-12Hz) to STN (13-35Hz/1Hz) PAC
% 37-->EEG(1-12Hz) to EEG (13-35Hz/1Hz) PAC

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clearvars -except Dat Subj;

% define scales
Init = 4; phys = 31;
load(['/Users/wanglinbin/Documents' ...
    '/LFPProject/RestAna/STN_Eyeclosed/EnppEEG.mat']);
i = 1;
subj_in = [];
for m = 1:size(Subj,2)
    if ~isnan(Dat{m,Init})
        subj_in(i) = m;
        i = i+1;
    end
end

for t = 1:size(subj_in,2)
    X(t) =  Dat{subj_in(1,t),Init};
    X1(t) =  Dat{subj_in(1,t),15};
    Y1(t,:) = Dat{subj_in(1,t),phys}./sum(Dat{subj_in(1,t),phys}(1:93));
    Y2(t,:) = Dat{subj_in(1,t),phys};
    for j = 1:7
        Y3(t,:,j) = Enpp{subj_in(1,t),(3*j-1)}(5);
    end
end
for i = 1:7
    [rho(i),pval(i)] = corr(X',Y3(:,:,i),'type','Spearman');
end%% Spearman correlation

Dataoutput = [pval',rho'];
% % %define the bands
delta = 3:5;
theta = 6:9;
alpha = 10:13;
lBeta = 14:21;
hBeta = 22:36;
lGamma = 42:62;
hGamma = 63:93;
Band = {delta,theta,alpha,lBeta,hBeta,lGamma,hGamma};

% Spearman correlation
for a = 1:size(Band,2)
    [rho1,pval1] = corr(zscore(Y3(:,:,1)),mean(Y1(:,Band{a}),2),'type','Pearson');
    r1(a) = rho1;
    p1(a) = pval1;
    [rho2,pval2] = corr(zscore(X)',mean(Y2(:,Band{a}),2),'type','Spearman');
    r2(a) = rho2;
    p2(a) = pval2;
   [rho3,pval3] = partialcorr(zscore(X)',mean(Y2(:,Band{a}),2),zscore(X1)','type','Spearman','Rows','complete');
    r3(a) = rho3;
    p3(a) = pval3;
end
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Spearman correlation
% clearvars -except Dat Subj;
% 
% % define scales
% Init = 12; phys = 28;
% 
% i = 1; 
% subj_in = [];
% for m = 1:size(Subj,2)
%     if ~isnan(Dat{m,Init})
%         subj_in(i) = m;
%         i = i+1;
%     end
% end
% 
% % frequencies
% for t = 1:size(subj_in,2)
%     X(t) = Dat{subj_in(1,t),Init};
%     Y1(t,:) = Dat{subj_in(1,t),25};
%     Y2(t,:) = Dat{subj_in(1,t),26};
%     Y3(t,:) = Dat{subj_in(1,t),27};
% end
% 
% % PAC
% delta = 1:4;
% theta = 4:8;
% alpha = 8:12;
% phase_band = {delta,theta,alpha};
% 
% for t = 1:size(subj_in,2)
%     Y(t,:,:) = Dat{subj_in(1,t),28}; % which electrophysiological data
% end
% 
% % % % correlation
% for i = 1:size(phase_band,2)
%         PAC = mean(mean(Y(:,phase_band{2},:),2),3);
% end
% 
% % % %define the bands
% delta = 3:5;
% theta = 6:9;
% alpha = 10:13;
% Beta = 14:36;
% Gamma = 42:93;
% Band = {delta,theta,alpha,Beta,Gamma};
% 
% x = [mean(Y1(:,Band{4}),2) mean(Y2(:,Band{2}),2) PAC];
% y = (X-1)';
% 
% 
% %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Spearman correlartion for one index
% clearvars -except Dat Enpp Subj;
% 
% % define scales
% Init = 4; phys = 27;
% % Spearman correlation
% i = 1;
% subj_in = [];
% for m = 1:size(Subj,2)
%     if ~isnan(Dat{m,Init})
%         subj_in(i) = m;
%         i = i+1;
%     end
% end
% for t = 1:size(subj_in,2)
%     X(t) = Dat{subj_in(1,t),Init};
%     Y(t,:) = Enpp{subj_in(1,t),phys};
% end
% 
% [rho,pval] = corr(zscore(X)',Y,'type','Spearman');



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% independent t test
clearvars -except Dat Subj EnppLFP;

% define scales
Init = 10; phy = 31; 

% define groups 
i = 1; j = 1;
GroupA = [];
GroupB = [];

for m = 1:size(Subj,2)

    if ~isnan(Dat{m,Init})

        if Dat{m,Init} == 1
            GroupA(i) = m;
            i = i+1;

        elseif Dat{m,Init} == 2
            GroupB(j) = m;
            j = j+1;

        end
    end
end


%% independent t test
% group A
for m = 1:size(GroupA,2)
    GA1(m,:) = Dat{GroupA(1,m),phy}./sum(Dat{GroupA(1,m),phy}(1:93))*100;
    GA2(m,:) = Dat{GroupA(1,m),phy};
end

GP1mean = mean(GA1,1);
GP1SD1 = mean(GA1,1)+std(GA1,1);
GP1SD2 = mean(GA1,1)-std(GA1,1);

% GP1mean = mean(GA2,1);
% GP1SD1 = mean(GA2,1)+std(GA2,1);
% GP1SD2 = mean(GA2,1)-std(GA2,1);

% group B
for n = 1:size(GroupB,2)
    GB1(n,:) = Dat{GroupB(1,n),phy}./sum(Dat{GroupB(1,n),phy}(1:93))*100;
    GB2(n,:) = Dat{GroupB(1,n),phy};
end

GP2mean = mean(GB1,1);
GP2SD1 = mean(GB1,1)+std(GB1,1);
GP2SD2 = mean(GB1,1)-std(GB1,1);

% GP2mean = mean(GB2,1);
% GP2SD1 = mean(GB2,1)+std(GB2,1);
% GP2SD2 = mean(GB2,1)-std(GB2,1);


% % % %define the bands
delta = 3:5;
theta = 6:9;
alpha = 10:13;
lBeta = 14:21;
hBeta = 22:36;
lGamma = 42:62;
hGamma = 63:93;
Band = {delta,theta,alpha,lBeta,hBeta,lGamma,hGamma};
%
% define group
group = [ones(1,size(GroupA,2)) ones(1,size(GroupB,2))*2];

for a = 1:93
    %% rel pow
    idx1 = GA1(:,a);
    idy1 = GB1(:,a);
    % idx1 = mean(GA1(:,Band{1,a}),2);
    % idy1 = mean(GB1(:,Band{1,a}),2);
    ind1 = [idx1;idy1];

    [p1,h1,stats1] = ranksum(idx1,idy1,'alpha',0.05);
    pvalue1(a) = p1;
    Z1(a) = stats1.zval;
    H1(a) = h1;
    %% abs pow
    idx2 = GA2(:,a);
    idy2 = GB2(:,a);
    % idx2 = mean(GA2(:,Band{1,a}),2);
    % idy2 = mean(GB2(:,Band{1,a}),2);
    ind2 = [idx2;idy2];
    % 方差齐性检验，即检验两组样本的总体方差是否相同
    % [p3,stats3] = vartestn(ind1,group,...
    %     'TestType','LeveneAbsolute','Display','off');
    % if p3<0.05
    %     disp('Equal variances not assumed') %方差不相同
    %     [h4,p4,ci4,stats4]=ttest2(idx2,idy2,...
    %         'Vartype','unequal');
    %     pvalue2(a) = p4;
    %     stat2(a) = stats4.tstat;
    % else
    %     disp('Equal variances assumed'); %方差相同
    %     [h4,p4,ci4,stats4]=ttest2(idx2,idy2);
    %     pvalue2(a) = p4;
    %     stat2(a) = stats4.tstat;
    % end
    [p2,h2,stats2] = ranksum(idx2,idy2,'alpha',0.05);
    pvalue2(a) = p2;
    Z2(a) = stats2.zval;
    H2(a) = h2;
end


for a = 1:size(Band,2)
    %% rel pow
    idx1 = mean(GA1(:,Band{1,a}),2);
    idy1 = mean(GB1(:,Band{1,a}),2);
    ind1 = [idx1;idy1];
    [p3,h3,stats3] = ranksum(idx1,idy1,'alpha',0.05);
    pvalue3(a) = p3;
    Z3(a) = stats3.zval;
    H3(a) = h3;


    %% abs pow
    idx2 = mean(GA2(:,Band{1,a}),2);
    idy2 = mean(GB2(:,Band{1,a}),2);
    ind2 = [idx2;idy2];

    [p4,h4,stats4] = ranksum(idx2,idy2,'alpha',0.05);
    pvalue4(a) = p4;
    Z4(a) = stats4.zval;
    H4(a) = h4;
end

Table = [GP1mean(1:93); GP1SD1(1:93); GP1SD2(1:93); GP2mean(1:93); GP2SD1(1:93); GP2SD2(1:93); pvalue1]';

idx2 = [mean(GA2(:,Band{1,1}),2) mean(GA2(:,Band{1,2}),2) mean(GA2(:,Band{1,3}),2) mean(GA2(:,Band{1,4}),2) mean(GA2(:,Band{1,5}),2) mean(GA2(:,Band{1,6}),2) mean(GA2(:,Band{1,7}),2)];
idy2 = [mean(GB2(:,Band{1,1}),2) mean(GB2(:,Band{1,2}),2) mean(GB2(:,Band{1,3}),2) mean(GB2(:,Band{1,4}),2) mean(GB2(:,Band{1,5}),2) mean(GB2(:,Band{1,6}),2) mean(GB2(:,Band{1,7}),2)];

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% indenpendent ttest for one index
clearvars -except Dat Subj;
% 
% % define scales
Init = 43; phy = 9;

% define groups
i = 1; j = 1;
GroupA = [];
GroupB = [];

for m = 1:size(Subj,2)

    if ~isnan(Dat{m,Init})&&~isnan(Dat{m,phy})

        if Dat{m,Init} == 1
            GroupA(i) = m;
            i = i+1;

        elseif Dat{m,Init} == 0
            GroupB(j) = m;
            j = j+1;

        end
    end
end


% group A
for m = 1:size(GroupA,2)
    GA2(m,:) = Dat{GroupA(1,m),phy};
end

% group B
for n = 1:size(GroupB,2)
    GB2(n,:) = Dat{GroupB(1,n),phy};
end

% define group
group = [ones(1,size(GroupA,2)) ones(1,size(GroupB,2))*2];
ind = [GA2;GB2];

[p2,h2,stats2] = ranksum(GA2,GB2,'alpha',0.05,'tail','right');



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loc = [];
clearvars -except Dat Subj loc;
for i = 1:size(Subj,2)

    Rx0(i,:) = (loc(i,1)+loc(i,4))/2;
    Ry0(i,:) = (loc(i,2)+loc(i,5))/2;
    Rz0(i,:) = (loc(i,3)+loc(i,6))/2;
    Rx1(i,:) = (loc(i,4)+loc(i,7))/2;
    Ry1(i,:) = (loc(i,5)+loc(i,8))/2;
    Rz1(i,:) = (loc(i,6)+loc(i,9))/2;
    Rx2(i,:) = (loc(i,7)+loc(i,10))/2;
    Ry2(i,:) = (loc(i,8)+loc(i,11))/2;
    Rz2(i,:) = (loc(i,9)+loc(i,12))/2;


    Lx0(i,:) = (loc(i,13)+loc(i,16))/2;
    Ly0(i,:) = (loc(i,14)+loc(i,17))/2;
    Lz0(i,:) = (loc(i,15)+loc(i,18))/2;
    Lx1(i,:) = (loc(i,16)+loc(i,19))/2;
    Ly1(i,:) = (loc(i,17)+loc(i,20))/2;
    Lz1(i,:) = (loc(i,18)+loc(i,21))/2;
    Lx2(i,:) = (loc(i,19)+loc(i,22))/2;
    Ly2(i,:) = (loc(i,20)+loc(i,23))/2;
    Lz2(i,:) = (loc(i,21)+loc(i,24))/2;
end

locR0 = [Rx0,Ry0,Rz0];
locL0 = [Lx0,Ly0,Lz0];
locR1 = [Rx1,Ry1,Rz1];
locL1 = [Lx1,Ly1,Lz1];
locR2 = [Rx2,Ry2,Rz2];
locL2 = [Lx2,Ly2,Lz2];

for i = 1:size(Subj,2)
    locT = [locR0(i,:); locR1(i,:);locR2(i,:);locL0(i,:);locL1(i,:);locL2(i,:)];
    Dat(i,44) = mat2cell(locT,size(locT,1),size(locT,2));
end

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clearvars -except Dat Subj
Init = 7;

i = 1; 
subj_in = [];
for m = 1:size(Subj,2)
    if ~isnan(Dat{m,Init}) 
        subj_in(i) = m;           
        i = i+1;
    end
end
 
% % %define the bands
delta = 3:5;
theta = 6:9;
alpha = 10:13;
lBeta = 14:21;
hBeta = 22:36;
lGamma = 42:62;
hGamma = 63:93;
Band = {delta,theta,alpha,lBeta,hBeta,lGamma,hGamma};

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% STN power
for a = 1: size(Band,2)
    raw = [];
    for i = 1:size(subj_in,2)
        for j = 1:size(Dat{subj_in(1,i),39},1)
            pow(j,:) = mean(Dat{subj_in(1,i),39}(j,Band{1,4}));%./sum(Dat{subj_in(1,i),39}(j,1:93)),2);
            score(j,:) = Dat{subj_in(1,i),Init};
            coord(j,:) = Dat{subj_in(1,i),40}(j,:);
            sub(j,:) = i;
            group(j,:) = 1;
        end
        subcoord = [pow score squeeze(coord(:,:)) sub group];
        raw = [raw;subcoord];
        clear subcoord pow score coord sub group
    end

    power= raw(:,1);
    scale = raw(:,2);
    [rho,pval] = corr(power,scale,'type','Spearman');
    r(a) = rho;
    p(a) = pval;
end


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% max power
for a = 1: size(Band,2)
    raw1 = [];
    for i = 1:size(subj_in,2)
        for j = 1:size(Dat{subj_in(1,i),38},1)
            pow(j,:) = mean(Dat{subj_in(1,i),38}(j,Band{1,4}));%./sum(Dat{subj_in(1,i),38}(j,1:93)),2);
            score(j,:) = Dat{subj_in(1,i),Init};
            coord(j,:) = Dat{subj_in(1,i),44}(j,:);
            sub(j,:) = i;
            group(j,:) = 2;
        end
        ind1 = find(pow == max(pow));
        subcoord = [pow(ind1) score(ind1) squeeze(coord((ind1),:)) sub(ind1) group(ind1)];
        raw1 = [raw1;subcoord];
        clear subcoord pow score coord sub group
    end

    power= raw1(:,1);
    scale = raw1(:,2);
    [rho,pval] = corr(power,scale,'type','Spearman');
    r(a) = rho;
    p(a) = pval;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% min power
% for a = 1: size(Band,2)
%     raw2 = [];
%     for i = 1:size(subj_in,2)
%         for j = 1:size(Dat{subj_in(1,i),38},1)
%             pow(j,:) = mean(Dat{subj_in(1,i),38}(j,Band{1,6})./sum(Dat{subj_in(1,i),38}(j,1:93)),2);
%             score(j,:) = Dat{subj_in(1,i),Init};
%             coord(j,:) = Dat{subj_in(1,i),44}(j,:);
%             sub(j,:) = i;
%             group(j,:) = 3;
%         end
%         ind2 = find(pow == min(pow));
%         subcoord = [pow(ind2) score(ind2) squeeze(coord((ind2),:)) sub(ind2) group(ind2)];
%         raw2 = [raw2;subcoord];
%         clear subcoord pow score coord sub group
%     end
% 
%     power= raw2(:,1);
%     scale = raw2(:,2);
%     [rho,pval] = corr(power,scale,'type','Spearman');
%     r(a) = rho;
%     p(a) = pval;
% end
% 
% sourcedata = [raw;raw1;raw2]; 


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% rand power
r = [];
p = [];
for k = 1:500
    for a = 1: size(Band,2)
        raw3 = [];
        for i = 1:size(subj_in,2)
            for j = 1:size(Dat{subj_in(1,i),38},1)
                pow(j,:) = mean(Dat{subj_in(1,i),38}(j,Band{1,a})./sum(Dat{subj_in(1,i),38}(j,1:93)),2);
                score(j,:) = Dat{subj_in(1,i),Init};
                coord(j,:) = Dat{subj_in(1,i),44}(j,:);
                sub(j,:) = i;
            end
            rands = randperm(size(Dat{subj_in(1,i),38},1));
            ind3 = rands(1);
            subcoord = [pow(ind3) score(ind3) squeeze(coord((ind3),:)) sub(ind3)];
            raw3 = [raw3;subcoord];
            clear subcoord pow score coord sub
        end

        power= raw3(:,1);
        scale = raw3(:,2);
        [rho,pval] = corr(power,scale,'type','Spearman');
        r(a,k) = rho;
        p(a,k) = pval;
    end
end

r_bdi =  0.3132; %0.3628; 
r_aes = 0.30; % nan
r_aesmax = 0.3149; % 0.2999
r_oci = -0.3920;%-0.3185; 
r_uppsmax = -0.3690;%-0.4022; 
r_upps =-0.3224; %-0.3734; 

r = r(:);
index = find(abs(r) > abs(r_oci));
permute_p = length(index)/3500;


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% power
for a = 1: size(Band,2)
    raw3 = [];
    for i = 1:size(subj_in,2)
        for j = 1:size(Dat{subj_in(1,i),38},1)
            pow(j,:) = mean(Dat{subj_in(1,i),38}(j,Band{1,6}),2);
            score(j,:) = Dat{subj_in(1,i),Init};
            coord(j,:) = Dat{subj_in(1,i),44}(j,:);
            sub(j,:) = i;
        end
        subcoord = [pow score coord sub];
        raw3 = [raw3;subcoord];
        clear subcoord pow score coord sub
    end

    power= raw3(:,1);
    scale = raw3(:,2);
    [rho,pval] = corr(power,scale,'type','Spearman');
    r(a) = rho;
    p(a) = pval;
end


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
values= power;
x = raw(:,3);
y = raw(:,4);
z = raw(:,5);

[rho1,pval1] = corr(sqrt(x.^2+y.^2),values,'type','Spearman');
[rho2,pval2] = corr(sqrt(x.^2+z.^2),values,'type','Spearman');
[rho3,pval3] = corr(sqrt(y.^2+z.^2),values,'type','Spearman');
[rho4,pval4] = corr(sqrt(y.^2+z.^2+x.^2),values,'type','Spearman');
[rho5,pval5] = corr(sqrt(x.^2),values,'type','Spearman');
[rho6,pval6] = corr(sqrt(y.^2),values,'type','Spearman');
[rho7,pval7] = corr(sqrt(z.^2),values,'type','Spearman');

r1(a) = rho1;
r2(a) = rho2;
r3(a) = rho3;
r4(a) = rho4;
r5(a) = rho5;
r6(a) = rho6;
r7(a) = rho7;

p1(a) = pval1;
p2(a) = pval2;
p3(a) = pval3;
p4(a) = pval4;
p5(a) = pval5;
p6(a) = pval6;
p7(a) = pval7;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PCA
clearvars -except Dat Subj;

% define scales
Init = 7; phys = 31;

i = 1; 
subj_in = [];
for m = 1:size(Subj,2)
    if ~isnan(Dat{m,Init})
        subj_in(i) = m;           
        i = i+1;
    end
end
 
for t = 1:size(subj_in,2)
    X(t) =  Dat{subj_in(1,t),Init};
    % X1(t) =  Dat{subj_in(1,t),15};
    Y1(t,:) = Dat{subj_in(1,t),phys}./sum(Dat{subj_in(1,t),phys}(1:93));
    Y2(t,:) = Dat{subj_in(1,t),phys};
end

%[rho1,pval1] = corr(zscore(X)',zscore(X1)','type','Spearman','rows','complete');
% % %define the bands
delta = 3:5;
theta = 6:9;
alpha = 10:13;
lBeta = 14:21;
hBeta = 22:36;
lGamma = 42:62;
hGamma = 63:93;
Band = {delta,theta,alpha,lBeta,hBeta,lGamma,hGamma};

% Spearman correlation
for a = 1:size(Band,2)
    [rho1,pval1] = corr(zscore(X)',mean(Y1(:,Band{a}),2),'type','Spearman');
    r1(a) = rho1;
    p1(a) = pval1;
    [rho2,pval2] = corr(zscore(X)',mean(Y2(:,Band{a}),2),'type','Spearman');
    r2(a) = rho2;
    p2(a) = pval2;
     %  [rho3,pval3] = partialcorr(X',mean(Y2(:,Band{a}),2),X1','type','Spearman','Rows','complete');
     % r3(a) = rho3;
     % p3(a) = pval3;
end
