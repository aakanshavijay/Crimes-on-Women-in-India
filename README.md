Crimes Against Women Data Explorer
This Shiny app provides a comprehensive, data-driven platform designed to explore and analyze crimes against women in India. It integrates machine learning models to predict future crime trends and offers a range of visualization options to better understand crime patterns across states and years.

Features:
Google Login: The app supports secure user authentication via OAuth 2.0, allowing users to access the dashboard and analysis tools after logging in with their Google account.
Dashboard: A dynamic dashboard presenting key crime statistics, including total reported cases, the most common crime types, and the states most affected by these crimes.
Data Explorer: This tool allows users to filter crime data by state, year, and crime type, making it easy to explore specific subsets of the dataset. Visualizations include line plots, bar charts, and more, with real-time updates based on the selected filters.
Visualizations Tab: Offers various chart types for in-depth analysis of crime trends, including Bar Charts, Line Charts, Heatmaps, and Scatter Plots. It provides flexibility for the user to customize the visualization by choosing the state, crime type, and time period.
Predictive Analysis: Users can predict future crime cases based on historical data. A Random Forest model is used for this prediction, allowing users to select a state, year, and crime type for forecasting future trends.


Technologies:
R & Shiny: The web application is built using R and Shiny, enabling interactive, real-time data visualization and user input handling.
Caret & Random Forest: Machine learning algorithms, specifically Random Forest, are used to create predictive models based on historical data, helping forecast future crime trends.
Google OAuth 2.0: Provides secure user authentication by integrating Google login for easy access.
ggplot2: For creating high-quality, interactive plots and charts to visualize crime trends.


Installation:
Clone this repository to your local machine.
Install the required R packages using the following command:
install.packages(c("shiny", "dplyr", "ggplot2", "caret", "httr"))
Run the app.R file to launch the Shiny app on your local machine.


Power BI Dashboard:
In addition to the Shiny app, a Power BI dashboard has been created to provide a rich visual representation of similar data, offering insights into crime trends, patterns, and predictions. The Power BI dashboard can be accessed separately for further analysis and visualization.


Future Improvements:
Extended Predictions: Incorporating additional crime types for more accurate and diversified predictions.
Data Sources: Expanding the dataset with more granular data points, such as district-level statistics.
User Experience: Enhancing the app’s interactivity with more advanced filtering options and a refined user interface.
