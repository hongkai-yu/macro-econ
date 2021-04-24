# A Machine Learning Approach to Equity Bubble Detection and Market Crash Prediction

# Introduction
In this research, I aim to detect equity bubbles and predict financial crashes in the S&P 500 index. I implement machine learning models with special consideration to the imbalanced data problem. The imbalanced data problem refers to the fact that there are far more non-bubbles than bubbles in the training data. The problem makes the model undesirably fits too much on non-bubbles. Features of the models are selected based on economic theories and market fundamentals. I find that the Random Forests model with Cross-Validation-tuned decision threshold performs the best, with a 93.0% balance accuracy rate. The balanced accuracy is the average of sensitivity (100%) and specificity (86%). The Recurrent Neural Networks with Bidirectional Long-term Short Memory and focal loss function gives unsatisfactory results, possibly due to the modest data size. Features with the highest predictive power are market fundamental indicators, long-term market returns, and the psychology factor. Short-term market returns and macroeconomic indicators are less important for the prediction of market crashes. The model in this research is limited to predicting long-term (more than 3 months) and extreme market downturns.

# Contents

- Research presentation slides are in _ECON 490 Research Slides_. The full paper is _Thesis.pdf_.
- The .R files are source code of the research except for the RNN analysis. The RNN analysis in _RNN.ipynb_.
- In the data folder,
  - bubble_detection.csv is the full data after data transformation and processing
  - dataset.md records the details of raw data in the folder
  - other .csv files are raw data for the research
  - Notice: much of the data are directly retrieved from the the web via R API, please see _prepareData.R_ for details
- The figures folder contains figures for illustrative use.
- The thesis folder contains the LaTeX code for the thesis and LaTeX tables.