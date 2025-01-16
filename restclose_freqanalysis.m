eeglab;

%% Prepare the dataset
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
EEGnodes = {'FP1','FP2','F3','F4','Fz','F7','F8'};
Dat = Subj';
% load LFPnodes
for m = 1:size(Subj,2)
    fprintf(['Enter channels of ', Subj{1,m},'\n']) ;
    chanIDs = unique(UI_cellArray(1, {}), 'stable') ;
    LFPnodes = chanIDs;
    Dat{m,2} = LFPnodes;
end

% load EEGnodes
Nodes = [];
EEGnodes = [];
for n = 30:size(Nodes,1)
    k = 1;
    for j = 1:size(Nodes,2)
        if ~isnan(Nodes(n,j))
            EEGnodes{k} = Nodes(n,j);
            k = k+1;
        end
    end
    Dat{n,3} = EEGnodes;
    clear EEGnodes;
end

save("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat","Dat");

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
load("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat.mat");
Lnodes = {'L0','L1','L2','R0','R1','R2'};
Rnodes = {'L0','L1','L2'};


for m = 1:size(Subj,2) 
    EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);
    if isnan(Dat{m,38})
        if size(EEG.data,1) == 13
            chanLFP = pop_select(EEG,'channel',Rnodes);
        else
            chanLFP = pop_select(EEG,'channel',Lnodes);
        end
    elseif m == 37
        chanLFP = pop_select(EEG,'channel',{'L0','L1','L2','R0','R1'});
    elseif m == 40
        chanLFP = pop_select(EEG,'channel',{'R0','R1','R2'});
    end

    chanLFP.data = zscore(chanLFP.data,0,2);

    for i = 1:size(chanLFP.data,1)
        [pow_wel,Freq_wel] = pwelch(chanLFP.data(i,:),512,0,512,500);
        pow(i,:) = pow_wel;
    end
    Dat(m,44) = mat2cell(pow,size(pow,1),size(pow,2));
    clear pow_wel chanLFP pow
end

save("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat","Dat");


figure;
chanLFP = pop_basicfilter(chanLFP,  1:3 , 'Boundary', 'boundary', 'Cutoff',  13, 'Design', 'butter', 'Filter', 'highpass', 'Order',  2, 'RemoveDC', 'on' );
chanLFP = pop_basicfilter(chanLFP,  1:3 , 'Boundary', 'boundary', 'Cutoff',  35, 'Design', 'butter', 'Filter', 'lowpass', 'Order',  2, 'RemoveDC', 'on' );
plot(chanLFP.times,chanLFP.data(1,:));

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% LFP_Freq_Analysis for two sides
% clc;clear;
% LoadPath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Continuous_data';
% File1 = dir(fullfile(LoadPath,'*.set'));
% Subj = {File.name};
% EEGnodes = {'FP1','FP2','F3','F4','Fz','F7','F8'};
% load("Demograph.mat");
%
% for m = 1:size(Subj,2)
%     EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);
%     for i = 1:size(Demograph{m,8},2)
%         if size(Demograph{m,8}) == 1
%             chanEEG = pop_select(EEG,'channel',Demograph{m,8});
%         elseif m == 9
%             chanEEG = pop_select(EEG,'channel',{Demograph{m,8}});
%         else
%             chanEEG = pop_select(EEG,'channel',{Demograph{m,8}{1,i}});
%         end
%         [pow_wel,Freq_wel] = pwelch(chanEEG.data,512,0,512,500);
%         if any(m == [5,6,7,11,23])
%             Demograph(m,10) = mat2cell(pow_wel',1); % left STN electrode power spectrum data
%         else
%             if i == 1
%                 Demograph(m,9) = mat2cell(pow_wel',1);  % right STN electrode power spectrum data
%             else
%                 Demograph(m,10) = mat2cell(pow_wel',1);
%             end
%         end
%         clear pow_wel chanEEG
%     end
% end
% save('D:\Rest_EEG_analysis\LFP_Freq_Analysis','Demograph');
%% LFP_Freq_Analysis when averaging both sides
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
load("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat.mat");

for m = 1:size(Subj,2)
    EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);
    if size(Dat{m,2}) == 1
        chanLFP = pop_select(EEG,'channel',Dat{m,2});
    else
        chanLFP = pop_select(EEG,'channel',{Dat{m,2}{1,1},Dat{m,2}{1,2}});
    end
    chanLFP.data = zscore(chanLFP.data,0,2);
    for i = 1:size(chanLFP.data,1)
        [pow_wel,Freq_wel] = pwelch(chanLFP.data(i,:),512,0,512,500);
        pow(i,:) = pow_wel;
    end
    Dat(m,40) = mat2cell(pow,size(pow,1),size(pow,2));
    clear pow_wel chanLFP pow
    % [pow_wel,Freq_wel] = pwelch(squeeze(mean(chanLFP.data,1)),512,0,512,500);% zscored or not
    % Dat(m,16) = mat2cell(Freq_wel',1);
    % Dat(m,17) = mat2cell(pow_wel',1);
    clear pow_wel chanLFP
end
save("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat","Dat");

%% EEG_Freq_Analysis
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
nodes = {'FP1','FP2','F3','F4','Fz','F7','F8'};
load("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat.mat");

for m = 30:size(Subj,2)
    EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);
    for j = 1:size(Dat{m,3},2)
        EEGnodes{j} = nodes{Dat{m,3}{1,j}};
    end
    EEG = pop_select(EEG,'channel',EEGnodes);
    EEG.data = zscore(EEG.data,0,2);
    [pow_wel,Freq_wel] = pwelch(squeeze(mean(EEG.data,1)),512,0,512,500);
    Dat(m,18) = mat2cell(pow_wel',1); % eeg power spectrum data
    clear EEGnodes
end

save("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat","Dat");

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Coherence
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
nodes = {'FP1','FP2','F3','F4','Fz','F7','F8'};
load("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat.mat");

for m = 30:size(Subj,2)
    EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);

    if size(Dat{m,2}) == 1
        chanLFP = pop_select(EEG,'channel',Dat{m,2});
    else
        chanLFP = pop_select(EEG,'channel',{Dat{m,2}{1,1},Dat{m,2}{1,2}});
    end
    chanLFP.data = zscore(chanLFP.data,0,2);
    chanLFP.data = squeeze(mean(chanLFP.data, 1));

    for j = 1:size(Dat{m,3},2)
        EEGnodes{j} = nodes{Dat{m,3}{1,j}};
    end
    chanEEG = pop_select(EEG,'channel',EEGnodes);
    chanEEG.data = zscore(chanEEG.data,0,2);
    chanEEG.data = squeeze(mean(chanEEG.data, 1));

    % set the sliding window and step size
    winSize = 30*500; % 30 second * 500 sample rate
    dataLength = size(EEG.data,2);
    stepvector = 1 : 0.5*winSize : dataLength; % 50% overlap
    for a = 1: size(stepvector,2)
        if (stepvector(a)+winSize-1) > dataLength
            break;
        end
        Wins(a,:) = stepvector(a):(stepvector(a)+winSize-1);
    end

    % calculate the coherence
    for i = 1:size(Wins,1)
        [cxy,fc] = mscohere(chanEEG.data(1,Wins(i,:)),chanLFP.data(1,Wins(i,:)),hann(512),256,[],500);
        Cxy(i,:) = cxy;
    end
    Cxy = squeeze(mean(Cxy,1))*100;
    %Demograph(m,13) = mat2cell(fc',1); % frequencies of coherence
    Dat(m,19) = mat2cell(Cxy,1); % coherence
    clear Wins EEGnodes
end

save("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat","Dat");


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculate entropy for time series
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
nodes = {'FP1','FP2','F3','F4','Fz','F7','F8'};
load("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat.mat");

Enpp = Subj';

% % %define the bands
delta = 1:4;
theta = 4:8;
alpha = 8:12;
lBeta = 13:35;
hBeta = 21:35;
lGamma = 40:60;
hGamma = 60:90;
Band = {delta,theta,alpha,lBeta,hBeta,lGamma,hGamma};

for j = 1:size(Band,2)

    for m = 1:size(Subj,2)

        % extract data
        EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);
        % LFP data
        if size(Dat{m,2}) == 1
            chanLFP = pop_select(EEG,'channel',Dat{m,2});
        else
            chanLFP = pop_select(EEG,'channel',{Dat{m,2}{1,1},Dat{m,2}{1,2}});
        end
        chanLFP.data = zscore(chanLFP.data,0,2);
        chanLFP.data = squeeze(mean(chanLFP.data, 1));

        %EEG data
        for k = 1:size(Dat{m,3},2)
            EEGnodes{k} = nodes{Dat{m,3}{1,k}};
        end
        chanEEG = pop_select(EEG,'channel',EEGnodes);
        chanEEG.data = zscore(chanEEG.data,0,2);
        chanEEG.data = squeeze(mean(chanEEG.data, 1));

       % filtering the frequency
        srate = 500;
        Af1 = min(Band{1,j});
        Af2 = max(Band{1,j});
        chanLFP.data = eegfilt(chanLFP.data,srate,Af1,Af2); % filtering
        set the sliding window and step size
        winSize = 2*500; % 2 second * 500 sample rate
        dataLength = size(EEG.data,2);
        stepvector = 1 : 0.5*winSize : dataLength; % 50% overlap
        for a = 1: size(stepvector,2)
            if (stepvector(a)+winSize-1) > dataLength
                break;
            end
            Wins(a,:) = stepvector(a):(stepvector(a)+winSize-1);
        end

        % calculate complexity
        for i = 1:size(Wins,1)
            % LFP
            [samp1,A1,B1] = SampEn(chanLFP.data(1,Wins(i,:)),"m",4,"r",0.1); % sample entropy
            [Perm1,Pnorm1,cPE1] = PermEn(chanLFP.data(1,Wins(i,:)),"m",4); % Permutation entropy
            [Ap1, Phi1] = ApEn(chanLFP.data(1,Wins(i,:)),"m",4,"r",0.1); % Approximate Entropy
            Perm_LFP(i,:) = samp1;
            PE_LFP(i,:) = Perm1;
            Ap_LFP(i,:) = Ap1;

        end

        Perm_LFP = squeeze(mean(Perm_LFP, 1)); 
        PE_LFP = squeeze(mean(PE_LFP, 1)); 
        Ap_LFP = squeeze(mean(Ap_LFP, 1));
        Enpp(m,(3*j-1)) = mat2cell(Perm_LFP,1);
        Enpp(m,(3*j)) = mat2cell(PE_LFP,1);
        Enpp(m,(3*j+1)) = mat2cell(Ap_LFP,1);

        clear Perm_LFP Perm_EEG PE_EEG PE_LFP Ap_EEG Ap_LFP Wins EEGnodes;
    end

end
save("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/EnppSpec","Enpp");
save("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat","Dat");
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculate Modulation Index (MI)
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
nodes = {'FP1','FP2','F3','F4','Fz','F7','F8'};
load("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat.mat");

%% Define the amplitude- and phase-frequencies

% 1-40Hz-> 40-90Hz
PhaseFreqVector=1:1:12;
AmpFreqVector=13:1:35;
PhaseFreq_BandWidth=2;
AmpFreq_BandWidth=20;

srate = 500;

%% Define phase bins
nbin = 18; % number of phase bins
position=zeros(1,nbin); % this variable will get the beginning (not the center) of each phase bin (in rads)
winsize = 2*pi/nbin;
for j=1:nbin
    position(j) = -pi+(j-1)*winsize;
end

for m = 1:size(Subj,2)
    EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);

    % LFP data
    if size(Dat{m,2}) == 1
        chanLFP = pop_select(EEG,'channel',Dat{m,2});
    else
        chanLFP = pop_select(EEG,'channel',{Dat{m,2}{1,1},Dat{m,2}{1,2}});
    end
    chanLFP.data = zscore(chanLFP.data,0,2);
    chanLFP.data = squeeze(mean(chanLFP.data, 1));

    % EEG data
    for j = 1:size(Dat{m,3},2)
        EEGnodes{j} = nodes{Dat{m,3}{1,j}};
    end
    chanEEG = pop_select(EEG,'channel',EEGnodes);
    chanEEG.data = zscore(chanEEG.data,0,2);
    chanEEG.data = squeeze(mean(chanEEG.data, 1));

    % set the sliding window and step size
    winSize = 30*500; % 30 second * 50 sample rate
    dataLength = size(EEG.data,2);
    stepvector = 1 : 0.5*winSize : dataLength; % 50% overlap
    for a = 1: size(stepvector,2)
        if (stepvector(a)+winSize-1) > dataLength
            break;
        end
        Wins(a,:) = stepvector(a):(stepvector(a)+winSize-1);
    end

    % set a empty matrix for saving data
    Comodulogram=single(zeros(size(Wins,1),length(PhaseFreqVector),length(AmpFreqVector)));

    for i = 1:size(Wins,1)

        AmpFreqTransformed = zeros(length(AmpFreqVector), winSize);
        PhaseFreqTransformed = zeros(length(PhaseFreqVector), winSize);

        %% hilbert transform (need eegfilt function)
        for ii=1:length(AmpFreqVector)
            Af1 = AmpFreqVector(ii);
            Af2=Af1+AmpFreq_BandWidth;
            AmpFreq=eegfilt(chanLFP.data(1,Wins(i,:)),srate,Af1,Af2); % filtering
            AmpFreqTransformed(ii, :) = abs(hilbert(AmpFreq)); % getting the amplitude envelope
        end

        for jj=1:length(PhaseFreqVector)
            Pf1 = PhaseFreqVector(jj);
            Pf2 = Pf1 + PhaseFreq_BandWidth;
            PhaseFreq=eegfilt(chanLFP.data(1,Wins(i,:)),srate,Pf1,Pf2); % filtering
            PhaseFreqTransformed(jj, :) = angle(hilbert(PhaseFreq)); % getting the phase time series
        end

        %% Caculate MI (need Modlindex_v2 function)
        counter1=0;
        for ii=1:length(PhaseFreqVector)
            counter1=counter1+1;

            Pf1 = PhaseFreqVector(ii);
            Pf2 = Pf1+PhaseFreq_BandWidth;

            counter2=0;
            for jj=1:length(AmpFreqVector)
                counter2=counter2+1;

                Af1 = AmpFreqVector(jj);
                Af2 = Af1+AmpFreq_BandWidth;
                [MI,MeanAmp]=ModIndex_v2(PhaseFreqTransformed(ii, :), AmpFreqTransformed(jj, :), position);
                Comodulogram(i,counter1,counter2)=MI;
            end
        end
    end

    PACdata = squeeze(mean(Comodulogram,1));
    Dat(m,49) = mat2cell(PACdata,size(PACdata,1)); % MI
    clear Wins
    %% Plot comodulogram
    %     clf
    %     contourf(PhaseFreqVector+PhaseFreq_BandWidth/2,AmpFreqVector+AmpFreq_BandWidth/2, Comodulogram',30,'lines','none')
    %     set(gca,'fontsize',14)
    %     ylabel('Amplitude Frequency (Hz)')
    %     xlabel('Phase Frequency (Hz)')
    %     colorbar
end
save("/Users/wanglinbin/Documents/LFPProject/RestAna/STN_Eyeclosed/Dat","Dat");

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculate PLI
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
nodes = {'FP1','FP2','F3','F4','Fz','F7','F8'};
load("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat.mat");

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculate PLV
clc;clear;
LoadPath =  ['D:\', 'Rest_EEG_analysis', '\Continuous_data\'];
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
load("analysisdata.mat");
band = inputdlg('the limits of band');%指定所要分析的频率的范围（单位是Hz）
band = str2num(band{1}); %将band变量由字符转换为数值

for m = 1:size(Subj,2)
    EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);

    % LFP data
    if size(Dat{m,2}) == 1
        chanLFP = pop_select(EEG,'channel',Dat{m,2});
    else
        chanLFP = pop_select(EEG,'channel',{Dat{m,2}{1,1},Dat{m,2}{1,2}});
    end
    chanLFP.data = zscore(chanLFP.data,0,2);
    chanLFP.data = squeeze(mean(chanLFP.data, 1));

    % EEG data
    for j = 1:size(Dat{m,3},2)
        EEGnodes{j} = nodes{Dat{m,3}{1,j}};
    end
    chanEEG = pop_select(EEG,'channel',EEGnodes);
    chanEEG.data = zscore(chanEEG.data,0,2);
    chanEEG.data = squeeze(mean(chanEEG.data, 1));

    %% 提取EEG在band频段的相位
    eeg_filtered = eegfilt(ALLEEG.data,...
        500,band(1,1),band(1,2),0,3*fix(500/band(1,1)),0,'fir1',0);
    bandPhase_eeg = angle(hilbert(eeg_filtered)); %逐个分段进行Hilbert变换，并提取相位

    %去掉数据中前10%和后10%的结果 因为hilbert变换对该区域不准确
    perc10w1 =  floor(size(bandPhase_eeg,2)*0.1);% 确定数据长度10%是多少个样本点
    bandPhase_eeg = bandPhase_eeg(:,perc10w1+1:end-perc10w1); %因Hilbert变换对数据首尾相位估算不准确，顾去掉前10%和后10%样本点的相位
    % epoch for bandphase
    % epoch_num1 = floor(size(band_phase,2)/size(EEG.data,2)); % 确定剩余的样本点如果转换为分段数据，可以分成多少段
    %     band_phase = band_phase(:,1:epoch_num1*size(EEG.data,2)); % 依据可以分成的段数，截取数据
    %     band_phase = reshape(band_phase,[size(EEG.data,1) size(EEG.data,2) epoch_num1]);% 将数据重新转换为二维：样本点*分段
    %% 提取LFP在band频段的相位
    lfp_filtered = eegfilt(ALLLFP.data,...
        500,band(1,1),band(1,2),0,3*fix(500/band(1,1)),0,'fir1',0);
    bandPhase_LFP = angle(hilbert(lfp_filtered)); %逐个分段进行Hilbert变换，并提取相位

    %去掉数据中前10%和后10%的结果 因为hilbert变换对该区域不准确
    perc10w2 =  floor(size(bandPhase_LFP,2)*0.1);% 确定数据长度10%是多少个样本点
    bandPhase_LFP = bandPhase_LFP(:,perc10w2+1:end-perc10w2); %因Hilbert变换对数据首尾相位估算不准确，顾去掉前10%和后10%样本点的相位
    % epoch for bandphase
    %     epoch_num2 = floor(size(band_phase_LFP,2)/size(LFP.data,2)); % 确定剩余的样本点如果转换为分段数据，可以分成多少段
    %     band_phase_LFP = band_phase_LFP(:,1:epoch_num2*size(LFP.data,2)); % 依据可以分成的段数，截取数据
    %     band_phase_LFP = reshape(band_phase_LFP,[size(LFP.data,2) epoch_num2]);% 将数据重新转换为三维：电极*样本点*分段

    % set the sliding window and step size
    winSize = 30*500; % 30 seconds * 50 sample rate
    dataLength = length(bandPhase_eeg);
    overlap = 0.5;
    stepvector = 1 : overlap*winSize : dataLength; % 50% overlap
    for a = 1: size(stepvector,2)
        if (stepvector(a)+winSize-1) > dataLength
            break;
        end
        Wins(a,:) = stepvector(a):(stepvector(a)+winSize-1);
    end

    %% 计算PLV
    for i = 1:size(Wins,1)
        x_phase = squeeze(bandPhase_eeg(1,Wins(i,:)));
        y_phase = squeeze(bandPhase_LFP(1,Wins(i,:)));
        rp = x_phase - y_phase;
        %%% PLV
        sub_plv(i) = abs(sum(exp(1i*rp))/length(rp)); % 计算某个分段的PLV
    end
    plv(m) = squeeze(mean(sub_plv,2)); % 对该被试各个分段的PLV计算平均值；
    clear Wins
    clear bandPhase_eeg bandPhase_LFP sub_pli sub_plv Wins
end

save("/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/Dat","Dat");
