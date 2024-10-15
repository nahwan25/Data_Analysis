import pandas as pd
import streamlit as st
import plotly.express as px

df = pd.read_csv('cleaned_data_dingling.csv')

df['datetime'] = pd.to_datetime(df['datetime'], errors='coerce')

st.sidebar.title("Navigation")
page = st.sidebar.radio("Go to", ["Interactive Plots", "Seasonal Pollutant Trends"])

seasons = ['Winter', 'Spring', 'Summer', 'Fall']
selected_season = st.sidebar.multiselect("Select Season", seasons, default=seasons)

df['date'] = df['datetime'].dt.date 
unique_dates = df['date'].unique()
min_date = unique_dates.min()
max_date = unique_dates.max()

selected_dates = st.sidebar.slider(
    "Select Date Range",
    min_value=min_date, 
    max_value=max_date,
    value=(min_date, max_date)
)

try:
    df_filtered = df[(df['season'].isin(selected_season)) & (df['date'].between(selected_dates[0], selected_dates[1]))]

    if df_filtered.empty:
        st.warning("No data available for the selected filters. Please adjust your filters.")
    else:
        if page == "Interactive Plots":
            st.title("Air Quality Dashboard - Interactive Plots")

            selected_hour = st.sidebar.selectbox('Select Hour:', df_filtered['hour'].unique())
            filtered_data = df_filtered[df_filtered['hour'] == selected_hour]

            st.write(f"### Data for Hour: {selected_hour}")
            st.write(filtered_data)

            st.write("## PM2.5 and PM10 Over Time")
            fig = px.line(df_filtered, x='datetime', y=['PM2.5', 'PM10'], labels={'value': 'Concentration (μg/m³)', 'datetime': 'Time'}, title='PM2.5 and PM10 Over Time')
            st.plotly_chart(fig)

            st.write("## Temperature Over Time")
            fig_temp = px.line(df_filtered, x='datetime', y='TEMP', labels={'TEMP': 'Temperature (°C)', 'datetime': 'Time'}, title='Temperature Over Time')
            st.plotly_chart(fig_temp)

            st.write("## Wind Speed Over Time")
            fig_wind = px.line(df_filtered, x='datetime', y='WSPM', labels={'WSPM': 'Wind Speed (m/s)', 'datetime': 'Time'}, title='Wind Speed Over Time')
            st.plotly_chart(fig_wind)

            st.write("## Data Summary")
            st.write(df_filtered.describe())

        elif page == "Seasonal Pollutant Trends":
            st.title("Air Quality Dashboard - Daily Averages with Seasonal Trends")

            df['season'] = pd.cut(df['datetime'].dt.month, 
                                  bins=[0, 3, 6, 9, 12], 
                                  labels=['Winter', 'Spring', 'Summer', 'Fall'],
                                  include_lowest=True)

            daily_avg = df_filtered.groupby(['date', 'season'])[['PM2.5', 'PM10', 'SO2', 'CO', 'O3']].mean().reset_index()

            pollutants = ['PM2.5', 'PM10', 'SO2', 'CO', 'O3']
            for pollutant in pollutants:
                st.write(f"### {pollutant} Trends by Season")
                fig = px.scatter(daily_avg, x='date', y=pollutant, color='season',
                                 labels={pollutant: 'Concentration', 'date': 'Date'},
                                 title=f'Trend {pollutant} Sepanjang Tahun')

                fig.add_traces(px.line(daily_avg, x='date', y=pollutant).data)
                st.plotly_chart(fig)

            st.write("## Data Summary")
            st.write(df_filtered.describe())

except (IndexError, ValueError) as e:
    st.error(f"An error occurred: {e}")
    st.warning("No data available for the selected filters.")