# loT-Based Virtual Health Determination


## Introduction

This is my project for the Big Data Analysis course I took in my senior year, focusing on Automated Vibration Analysis of Robotic Arm and Dashboard Implementation using R language and python. This project aims to help factories determine when a machine requires maintenance (i.e., assess the health of the machinery). However, due to limitations in the data, we can only determine the health of the machinery through the vibration signals of the robotic arm, and cannot predict in advance whether the machine is about to fail.

## Data Source

The data is provided by a professor and is confidential, so the dataset will not be made public.


## Tools and statistical methods Used

- **R language**:
- **Python**: 
- **statistical methods**:
1. Exploratory Data Analysis(EDA): Visualized the data, Descriptive statistics, Detected outliers
2. Data Preprocessing: Removed outliers,  Fast Fourier Transform(FFT), Feature Engineering, Principal Component Analysis(PCA)
3. Hypothesis testing: Kruskal–Wallis test, Fisher Discriminant Score
4. Established Model: Random Forest
5. Selected Model: by Accuracy
6. System Design: Designed DashBoard by R packages-Shiny


## Project Structure

```plaintext
BigData_course/
├── R/                     # Folder containing R code files.
│   ├── app.R              # Chinese version dashboard
│   ├── app_en.R           # English version dashboard
│   └── requirements.R
├── python/                # Folder containing Python code files.
│   ├── module.py
│   └── trained_models/.   # Folder containing models which were trained by us already.
│       ├── Xa_model_24.joblib
│       ├── Xb_model_20.joblib
│       ├── Ya_model_4.joblib
│       └── Yb_model_20.joblib
├── environment.yml
├── report/                # Folder containing the project report details in .docx format and .pdf format.
│   ├── group2_Chinese.docx
│   └── group2_Chinese.pdf
├── test_data/             # The data is intended to be used in the demo within the dashboard.
│   ├── Xa/
│       ├── gvb000002.txt
│       ├── gvb000003.txt
│       └── gvb000004.txt
│   ├── Xb/
│       ├── gvb000006.txt
│       ├── gvb000007.txt
│       └── gvb000008.txt
│   ├── Ya/
│       ├── gvb000012.txt
│       ├── gvb000013.txt
│       └── gvb000014.txt
│   └── Yb/
│       ├── gvb000050.txt
│       ├── gvb000051.txt
│       └── gvb000052.txt
├── fonts/                # Folder containing fonts.
│   ├── PingFang.ttc
└── README.md              # This file.
```



## How to Use

## Setting up the Environment

1. Clone the repository(you may need to install git at first):
    ```bash
    git clone https://github.com/Iris910531/BigData_course.git
    cd BigData_course
    ```

2. Create the Conda environment(you may need to install conda at first):
    ```bash
    conda env create -f environment.yml
    ```

3. Activate the environment:
    ```bash
    conda activate bigdata
    ```

4. Install the required R packages:
    ```r
    # In your R session
    source("requirements.R")
    ```
5. Run the Shiny app:
    ```r
    # In your R session
    source("R/app.R")
    # you can also use app_en depends on the language you want
    ```
## Notes

If you encounter error messages in Step 4, please change the file paths to absolute paths.
If you encounter error messages in Step 5, please open the file `R/app.R` directly and replace the following paths with their absolute paths before running the file:
- `source_python("python/module.py")`
- `"/python/trained_models/Xa_model_24.joblib"`
- `"/python/trained_models/Xb_model_20.joblib"`
- `"/python/trained_models/Ya_model_4.joblib"`
- `"/python/trained_models/Yb_model_20.joblib"`
- `font_add("PingFang", "/fonts/PingFang.ttc")`

## Using the Dashboard

1. **Select the actuation position**: Click on the dropdown menu and choose the desired position from the options: Xa, Xb, Ya, or Yb.
2. **Upload files**: Click on the "Browse" button to upload your files. Note that you can upload up to 25 files at a time. If you try to upload more than 25 files, the process will fail and no results will be displayed.
3. **Click "Process Files"**: After selecting the actuation position and uploading the files, click on the "Process Files" button to start the analysis.
4. **View the results**: The pie chart and the grid plot on the right will display the vibration conditions of the robotic arm based on the uploaded data.

## Notes

- Ensure that the uploaded files are in the correct format (.txt).
- If the selected actuation position does not match the uploaded data, an error message will be displayed indicating the mismatch.
- The pie chart will show the distribution of different vibration conditions, while the grid plot will provide a detailed view of each file's results.

For further assistance, please contact Mr. Li. Phone: 0912345678 Email: abc@gmail.com
(fake contact information, just for real usage scenarios)

## Contact Information

For any further questions or collaboration opportunities, please reach out to me at:
- Email: [yguo8395@gmail.com](mailto:yguo8395@gmail.com)
- LinkedIn: [Iris Kuo](https://www.linkedin.com/in/yi-hsuan-kuo-835b00268/))
- GitHub: [Iris Kuo](https://github.com/Iris910531)