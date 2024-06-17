import os 
import glob 
import numpy as np 
import pandas as pd 
from numpy.fft import fft
import joblib
import re

def compute_fft_for_grouped_data(data):
    def compute_fft(group):
        fft_result_x = np.fft.fft(group['Xaxis'].values)
        fft_result_y = np.fft.fft(group['Yaxis'].values)
        fft_result_z = np.fft.fft(group['Zaxis'].values)
        
        freqs = np.fft.fftfreq(len(group))
        
        group['FFTx'] = fft_result_x
        group['FFTy'] = fft_result_y
        group['FFTz'] = fft_result_z
        group['Frequency'] = freqs
        group = group.query("Frequency >= 0 and Frequency <= 0.5")
        return group

    grouped_data = data.groupby(['Segment_Head', 'Segment_Tail'])
    fft_results = grouped_data.apply(compute_fft)
    fft_results = fft_results.reset_index(drop=True)

    # 分別計算每個 FFT 結果的振幅並存储
    fft_results['FFTx_Amplitude'] = fft_results['FFTx'].apply(lambda x: np.abs(x) if pd.notnull(x) else np.nan)
    fft_results['FFTy_Amplitude'] = fft_results['FFTy'].apply(lambda x: np.abs(x) if pd.notnull(x) else np.nan)
    fft_results['FFTz_Amplitude'] = fft_results['FFTz'].apply(lambda x: np.abs(x) if pd.notnull(x) else np.nan)

    return fft_results

def slice_maxium(data :"DataFrame" , num: "int"):
    '''
    data : compute_fft_for_grouped_data()的return DataFrame 
    num : 要是int，對應位置最適合的切片數
    -------------------------------------------------------
    return 為切片後處理DataFrame
    '''
    slice_index = np.linspace(0, 0.5, num)[1:num]
    total_data = pd.DataFrame()
    example_1 = data.query(f"Frequency < {slice_index[0]}").loc[:,["Segment_Head","Segment_Tail","FFTx_Amplitude", "FFTy_Amplitude","FFTz_Amplitude"]]\
            .groupby(["Segment_Head","Segment_Tail"]).apply(lambda x:x.max())
    example_1 = pd.DataFrame(example_1)
    new_names = {"FFTx_Amplitude": "FFTx_Amplitude"+str(1),
                 "FFTy_Amplitude": "FFTy_Amplitude"+str(1),
                 "FFTz_Amplitude": "FFTz_Amplitude"+str(1)}
    example_1 = example_1.reset_index(drop=True).rename(columns = new_names)

    for i in range(1, len(slice_index)): #i是切點  
        example_2 = data.query(f"Frequency < {slice_index[i]} and Frequency > {slice_index[i-1]}").loc[:,["Segment_Head","Segment_Tail","FFTx_Amplitude", "FFTy_Amplitude","FFTz_Amplitude"]]\
        .groupby(["Segment_Head","Segment_Tail"]).apply(lambda x:x.max())
        example_2 = pd.DataFrame(example_2)
        new_names = {"FFTx_Amplitude": "FFTx_Amplitude"+str(i+1),
                     "FFTy_Amplitude": "FFTy_Amplitude"+str(i+1),
                     "FFTz_Amplitude": "FFTz_Amplitude"+str(i+1)}
        example_2 = example_2.reset_index(drop=True).rename(columns = new_names)
        example_1 = pd.merge(left=example_1, right=example_2, how="inner", on =['Segment_Head', 'Segment_Tail'])
    return example_1


def input_data(data : "DataFrame", pos : "str", models_path:"path", name:"str" = "空檔名"):
    '''
    data: 郭處理後的資料
    pos: 位置("Xa", "Xb", "Ya", "Yb")
    models_path: 四個位置模型的資料夾路徑
    name: 檔名
    '''
    if pos not in ("Xa", "Xb", "Ya", "Yb"):
        raise Exception("位置輸入有誤(請輸入Xa, Xb, Ya, Yb)")
    if isinstance(data, pd.DataFrame) == False:
        return ("資料輸入不為資料表", 0)
    if isinstance(name, str) == False:
        raise Exception("檔名輸入不為字串")


    dic_num = {"Xa":24, "Xb" : 20, "Ya":4, "Yb":20 }
    dic_chi = {"Xa":"水平作動馬達側", "Xb" : "水平作動惰輪側", "Ya":"垂直作動馬達側", "Yb":"垂直作動惰輪側" }
    fft_data = compute_fft_for_grouped_data(data)
    sliced_data = slice_maxium(fft_data, dic_num[pos])
        
    model = joblib.load(models_path)

    pre_result = model.predict(sliced_data)
    
    
    if np.unique(pre_result).shape[0] != 1:
        return (f"位置: {dic_chi[pos]} 檔名:{name} 預測結果不一，請通知公司進行檢查 ! "), 0
    elif np.unique(pre_result).shape[0] == 1:
        pre_strength = int("".join(re.findall(r"\d" , np.unique(pre_result)[0])))
        if pre_strength != (80*(pos in ("Xa", "Xb")) + 260*(pos in ("Ya", "Yb"))):
            return (f"位置: {dic_chi[pos]} 檔名:{name} 檢測結果為異常狀態，施壓力為{pre_strength}左右"), pre_strength
        else:
            return (f"位置: {dic_chi[pos]} 檔名:{name} 檢測結果為健康狀態"), pre_strength

