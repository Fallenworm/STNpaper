%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load raw data
clc;clear;
LoadPath = 'D:/STN_Eyeclosed/Raw_data/';
SavePath = 'D:/STN_Eyeclosed/Filtered_data/';
File = dir(fullfile(LoadPath,'*.vhdr'));
Subj = {File.name};
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Filtering
for  m = 1 : size(Subj,2)
   % m = 1;
    EEG = pop_loadbv(LoadPath, Subj{1,m}, [], []);
    EEG = eeg_checkset(EEG);
    %% Bipolar offline
    if any(m == 6:9)
        for i = 1:3
            LFP = EEG.data(i+1,:) - EEG.data(i,:);
            EEG.data(i,:,:) = LFP;
        end
        EEG = pop_select( EEG,'channel',{'L0' 'L1' 'L2', 'FP1' 'FP2' 'Fz' 'F3' 'F4' 'F7' 'F8' 'HEOG' 'hVEOG' 'lVEOG'});
    else
        for j = 1:3
            L_LFP = EEG.data(j+1,:) - EEG.data(j,:);
            EEG.data(j,:,:) = L_LFP;
        end
        for k = 5:7
            R_LFP = EEG.data(k+1,:) - EEG.data(k,:);
            EEG.data(k,:,:) = R_LFP;
        end
        EEG = pop_select(EEG,'channel',{'L0' 'L1' 'L2' 'R0' 'R1' 'R2','FP1' 'FP2' 'Fz' 'F3' 'F4' 'F7' 'F8' 'HEOG' 'hVEOG' 'lVEOG'});
    end

    %% Filter 
    if any(m == 6:9)
        EEG  = pop_basicfilter( EEG,  1:13 , 'Boundary', 'boundary', 'Cutoff',  1, 'Design', 'butter', 'Filter', 'highpass', 'Order',  2, 'RemoveDC', 'on' );
    elseif any(m == 1)
        EEG  = pop_basicfilter( EEG,  1:16 , 'Boundary', 'boundary', 'Cutoff',  50, 'Design', 'notch', 'Filter', 'PMnotch', 'Order',  180, 'RemoveDC', 'on' );
        EEG  = pop_basicfilter( EEG,  1:16 , 'Boundary', 'boundary', 'Cutoff',  1, 'Design', 'butter', 'Filter', 'highpass', 'Order',  2, 'RemoveDC', 'on' );
    else
        EEG  = pop_basicfilter( EEG,  1:16 , 'Boundary', 'boundary', 'Cutoff',  1, 'Design', 'butter', 'Filter', 'highpass', 'Order',  2, 'RemoveDC', 'on' );
    end

    %% Save datasets
    pop_saveset(EEG, 'filename',Subj{1,m},'filepath',SavePath);
    clear EEG L_LFP R_LFP
end
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ICA
clc;clear;
LoadPath =  'D:/STN_Eyeclosed/LocatedData/';
SavePath =  'D:/STN_Eyeclosed/ICAData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
for  m = 1
    EEG = pop_loadset('filename',Subj{1, m},'filepath',LoadPath);
    EEG = eeg_checkset( EEG );
    EEG = pop_chanedit(EEG, 'lookup','C:\toolbox\eeglab2023.0\plugins\dipfit5.2\standard_BEM\elec\standard_1005.elc');
    EEG = pop_runica(EEG, 'icatype', 'runica','extended',1,'interrupt','on','chanind', [7 8 9 10 11 12 13 14 15 16]);
    EEG = pop_saveset(EEG, 'filename',Subj{1,m},'filepath',SavePath);
end
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
clc;clear;
LoadPath =  'D:/STN_Eyeclosed/ICAData/';
SavePath =  'D:/STN_Eyeclosed/SegmentedData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
for  m = 7
    EEG = pop_loadset('filename',Subj{1, m},'filepath',LoadPath);
    EEG = eeg_checkset( EEG );
    EEG = pop_resample( EEG, 500);
    EEG.event = [];
    EEG = eeg_regepochs(EEG, 'recurrence', 2, 'limits',[0 2], 'rmbase',NaN);
    EEG = pop_saveset(EEG, 'filename',Subj{1,m},'filepath',SavePath);
end

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Merge epoched data
clc;clear;
LoadPath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/SegmentedData/';
SavePath =  '/Users/wanglinbin/Documents/LFPProject/Rest_EEG_analysis/STN_Eyeclosed/20231123FinalData/';
File = dir(fullfile(LoadPath,'*.set'));
Subj = {File.name};
for  m = 1 : size(Subj,2)
    EEG = pop_loadset('filename',Subj{1,m},'filepath',LoadPath);
    for i = 1:length(EEG.epoch)
        ALLEEG(i) = pop_selectevent( EEG, 'epoch',i,'deleteevents','off','deleteepochs','on','invertepochs','off'); 
    end
    EEG = pop_mergeset( ALLEEG, 1:length(EEG.epoch), 0); 
    EEG.event = []; 
    EEG = pop_saveset(EEG, 'filename',Subj{1,m},'filepath',SavePath);
end

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%