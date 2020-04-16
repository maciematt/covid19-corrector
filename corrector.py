#!/usr/bin/env python3




import numpy as np
import pandas as pd

import io
import requests

import plotly.offline as py
#py.init_notebook_mode(connected=True)
#py.init_notebook_mode()
import plotly.graph_objs as go
from plotly import tools
#import plotly.figure_factory as ff
import seaborn as sns

import warnings
warnings.filterwarnings('ignore')

import matplotlib.pyplot as plt
plt.style.use('seaborn-whitegrid')

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

import os
for dirname, _, filenames in os.walk('kaggle/input'):
    for filename in filenames:
        print(os.path.join(dirname, filename))

# Any results you write to the current directory are saved as output.






class Corrector:

    def __init__(self):

        confirmed_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
        deaths_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
        recovered_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'

        s = requests.get(confirmed_url).content
        cases = pd.read_csv(io.StringIO(s.decode('utf-8')))

        s = requests.get(deaths_url).content
        deaths = pd.read_csv(io.StringIO(s.decode('utf-8')))

        s = requests.get(recovered_url).content
        recovered = pd.read_csv(io.StringIO(s.decode('utf-8')))


        cases['Country/Region'] = cases['Country/Region'].replace('Iran (Islamic Republic of)', 'Iran')
        cases['Country/Region'] = cases['Country/Region'].replace('Taiwan*', 'Taiwan')
        cases['Country/Region'] = cases['Country/Region'].replace('Criuse Ship', 'Diamond Princess')
        cases['Country/Region'] = cases['Country/Region'].replace('Korea, South', 'South Korea')

        deaths['Country/Region'] = deaths['Country/Region'].replace('Iran (Islamic Republic of)', 'Iran')
        deaths['Country/Region'] = deaths['Country/Region'].replace('Taiwan*', 'Taiwan')
        deaths['Country/Region'] = deaths['Country/Region'].replace('Criuse Ship', 'Diamond Princess')
        deaths['Country/Region'] = deaths['Country/Region'].replace('Korea, South', 'South Korea')

        recovered['Country/Region'] = recovered['Country/Region'].replace('Iran (Islamic Republic of)', 'Iran')
        recovered['Country/Region'] = recovered['Country/Region'].replace('Taiwan*', 'Taiwan')
        recovered['Country/Region'] = recovered['Country/Region'].replace('Criuse Ship', 'Diamond Princess')
        recovered['Country/Region'] = recovered['Country/Region'].replace('Korea, South', 'South Korea')

        countries = cases.iloc[:,1].unique()
        countries.sort()


        self.cases = cases
        self.deaths = deaths
        self.recovered = recovered
        self.countries = countries



        demo = pd.read_csv('kaggle/input/world_demographics.csv')

        demo['Country or Area'] = demo['Country or Area'].replace('Viet Nam', 'Vietnam')
        demo['Country or Area'] = demo['Country or Area'].replace('United States of America', 'US')
        demo['Country or Area'] = demo['Country or Area'].replace('United Kingdom of Great Britain and Northern Ireland', 'United Kingdom')
        demo['Country or Area'] = demo['Country or Area'].replace('Republic of Korea', 'South Korea')
        demo['Country or Area'] = demo['Country or Area'].replace('Venezuela (Bolivarian Republic of)', 'Venezuela')
        demo['Country or Area'] = demo['Country or Area'].replace('Iran (Islamic Republic of)', 'Iran')

        self.demo = demo




    def plot_country(self, country):

        assert country in self.countries, 'Selected country {} not available! Available options: {}'.format(country, self.countries)


        # plt.figure(figsize=(16,30))
        # plt.tight_layout(pad=3.0)
        plt.figure()

        # plt.rcParams.update({'font.size': 26})


        c_ma = np.where(self.cases['Country/Region'] == country)[0]
        d_ma = np.where(self.deaths['Country/Region'] == country)[0]
        r_ma = np.where(self.recovered['Country/Region'] == country)[0]


        c_count = self.cases.iloc[c_ma, 4:].sum(axis=0)
        d_count = self.deaths.iloc[d_ma, 4:].sum(axis=0)
        r_count = self.recovered.iloc[r_ma, 4:].sum(axis=0)


        sig_cases = np.where(c_count > 0)[0]
        # ax1 = plt.subplot(10, 3, counter)
        ax1 = plt.subplot()
        res1, = plt.plot(c_count[sig_cases], 'ro-', linewidth=3, label='reported cases')
        res2, = plt.plot(r_count[sig_cases], 'b^-', linewidth=3, label='recovered')
        plt.xticks(rotation=90)
        ax2 = ax1.twinx()
        res3, = ax2.plot(d_count[sig_cases], 'gs-', linewidth=3, label='deaths')
        
        plt.title(country)
        ax1.set_ylabel('cases', color="black")
        color = 'green'
        ax2.set_ylabel('deaths', color=color)
        ax2.tick_params(axis='y', labelcolor=color)
        plt.legend(handles=[res1, res2, res3])


        # plt.subplots_adjust(left=0, right=2, top=2, bottom=0.5, hspace=1)
        plt.show()





    def new_correction(self, active_country='China', reference_country='South Korea'):


        assert active_country in self.countries, "Selected country {} not available! Available options: {}".format(active_country, self.countries)
        assert reference_country in self.countries, "Selected country {} not available! Available options: {}".format(reference_country, self.countries)


        self.active_country = active_country
        ma = np.where(self.cases["Country/Region"] == self.active_country)[0]
        country1_count = self.cases.iloc[ma, 4:].sum(axis=0)
        country1_deaths = self.deaths.iloc[ma, 4:].sum(axis=0)
        self.dr_act = country1_deaths / country1_count

        self.d_cases_act = np.where(country1_deaths > 3)[0]
        # d_cases = np.where(country_deaths > 0)[0]

        self.reference_country = reference_country
        ma = np.where(self.cases['Country/Region'] == self.reference_country)[0]
        country0_count = self.cases.iloc[ma, 4:].sum(axis=0)
        country0_deaths = self.deaths.iloc[ma, 4:].sum(axis=0)
        self.dr_ref = country0_deaths / country0_count
        self.d_cases_ref = np.where(country0_deaths > 3)[0]



        deathrate_age = list(range(0,120))

        deathrate_age[0:30]   = [0]*30
        deathrate_age[30:40]  = [0.0011]*10
        deathrate_age[40:50]  = [0.0009]*10
        deathrate_age[50:60]  = [0.0037]*10
        deathrate_age[60:70]  = [0.0151]*10
        deathrate_age[70:80]  = [0.0535]*10
        deathrate_age[80:120] = [0.1084]*40


        ma = np.where(self.demo['Country or Area'] == self.active_country)[0]
        self.demo_act = self.demo.iloc[ma,:]
        max_year = np.where(self.demo_act['Year'] == self.demo_act['Year'].max())[0]
        self.demo_act = self.demo_act.iloc[max_year]

        ma = np.where(self.demo['Country or Area'] == self.reference_country)[0]
        self.demo_ref = self.demo.iloc[ma,:]
        max_year = np.where(self.demo_ref['Year'] == self.demo_ref['Year'].max())[0]
        self.demo_ref = self.demo_ref.iloc[max_year]


        ll = list(range(0,120))
        ll = [str(i) for i in ll]


        ma = np.where(np.isin(self.demo_act['Age'], np.array(ll)))[0]
        self.demo_act = self.demo_act.iloc[ma,:]

        ma = np.where(np.isin(self.demo_ref['Age'], np.array(ll)))[0]
        self.demo_ref = self.demo_ref.iloc[ma,:]


        self.vulnerable_act = [a * b for a, b in zip(self.demo_act['Value'], deathrate_age)]
        self.vulnerable_ref = [a * b for a, b in zip(self.demo_ref['Value'], deathrate_age)]


        self.v_act = sum(self.vulnerable_act)/sum(self.demo_act['Value'])
        self.v_ref = sum(self.vulnerable_ref)/sum(self.demo_ref['Value'])


        exp_diff = self.v_act/self.v_ref


        self.scaling_factor = self.dr_act[self.d_cases_act] / (exp_diff*sum(self.dr_ref[self.d_cases_ref])/len(self.d_cases_ref))
        self.potential_cases = country1_count[self.d_cases_act] * self.scaling_factor






    def stat_fix(self, 
                 death_fix_loc=5, death_fix_sh=9, death_fix_sc=1, death_fix_type='normal', 
                 cases_fix_loc=5, cases_fix_sh=9, cases_fix_sc=1, cases_fix_type='normal'
                 ):

        """
        Here two different types of distributions are available - gamma or normal; since gamma is a little less in-
        tuitive, you can figure out the mean as shape*scale (here dr_fix_sh*dr_fix_sc), and SD is sqrt(shape*scale^2)
        """

        assert death_fix_type in ['normal', 'gamma']
        assert cases_fix_type in ['normal', 'gamma']


        ma = np.where(self.deaths["Country/Region"] == self.active_country)[0]



        ## Draw distribution and fix the number of death -------------------- ##

        ## Number of deaths here is fixed using just "stats"; don't have data for how much underreported deathes wind
        ## up, so using only priors; could use data if I can find it, though


        if death_fix_type == 'normal':
            death_fix_prior = np.random.normal(death_fix_loc, death_fix_sc, 1000)
        elif death_fix_type == 'gamma':
            death_fix_prior = np.random.gamma(death_fix_sh, death_fix_sc, 1000)


        _ = self.deaths.iloc[ma, 4:].sum(axis=0)
        death_orig = pd.DataFrame({'day': range(0, len(_)), 'value': _})
        death_orig = death_orig.iloc[np.where(death_orig.value > 3)[0], :]


        corrected_death = pd.melt(pd.concat([death_orig[['day']].reset_index(drop=True), pd.DataFrame(np.outer(death_orig.value, death_fix_prior), index=death_orig.index).reset_index().rename(columns={'index': 'date'})], axis=1), id_vars=['date', 'day'])
        corrected_death['date'] = pd.to_datetime(corrected_death['date'])


        self.death_orig = death_orig
        self.corrected_death = corrected_death


        ## ------------------------------------------------------------------ ##




        ## Fixing "cases" --------------------------------------------------- ##

        ## In fixing the estimates of cases there are two sources that need correcting - "delibarate" or "semi-delibe-
        ## rate" underreporting, AND the fact that some of the patients are asymptomatic or never get tested. For now
        ## not splitting these two sources but rather just using a unified estimator here. Also prior-only for now, 
        ## until I find some data to use here.


        if cases_fix_type == 'normal':
            cases_fix_prior = np.random.normal(cases_fix_loc, cases_fix_sc, 1000)
        elif cases_fix_type == 'gamma':
            cases_fix_prior = np.random.gamma(cases_fix_sh, cases_fix_sc, 1000)


        cases_orig = pd.concat([death_orig[['day']].tail(len(self.potential_cases)), pd.DataFrame(self.potential_cases, columns=['values'])], axis=1).rename(columns={'values': 'value'})


        corrected_cases = pd.melt(pd.concat([cases_orig[['day']].reset_index(drop=True), pd.DataFrame(np.outer(cases_orig.value, cases_fix_prior), index=cases_orig.index).reset_index().rename(columns={'index': 'date'}).reset_index(drop=True)], axis=1), id_vars=['date', 'day'])
        corrected_cases['date'] = pd.to_datetime(corrected_cases['date'])


        self.cases_orig = cases_orig
        self.corrected_cases = corrected_cases


        ## ------------------------------------------------------------------ ##





    def plot_fixed_death(self):

        sns.lineplot(data=self.corrected_death, x='day', y='value', color='red', ci='sd')
        sns.lineplot(data=self.death_orig, x='day', y='value', color='blue')





    def plot_fixed_cases(self):

        sns.lineplot(data=self.corrected_cases, x='day', y='value', color='red', ci='sd')
        sns.lineplot(data=self.cases_orig, x='day', y='value', color='blue')





    def compare_deathrates(self):


        plt.figure(figsize=(10,6))
        plt.rc('ytick', labelsize=26) 
        plt.rc('xtick', labelsize=13)

        res1, = plt.plot(self.dr_act[self.d_cases_act]*100, 'b.-', label='Death rate ({})'.format(self.active_country))
        res2, = plt.plot(self.dr_ref[self.d_cases_act]*100, 'r.-', label='Death rate ({})'.format(self.reference_country))

        plt.ylabel('death rate')
        plt.xticks(rotation=90)
        plt.legend(handles=[res1, res2])
        plt.show()



        plt.figure(figsize=(5,6))
        x = [self.dr_act[self.d_cases_act]*100,
             self.dr_ref[self.d_cases_ref]*100]
        plt.boxplot(x)
        plt.xticks([1, 2], [self.active_country, self.reference_country])
        plt.show()





    def compare_demo(self):



        p_c1 = self.demo_act['Value']
        p_c2 = self.demo_ref['Value']

        y = list(range(0, 100, 1))

        layout = go.Layout(yaxis=go.layout.YAxis(title='Age'),
                           xaxis=go.layout.XAxis(
                               range=[-(p_c1+p_c2).max(), (p_c1+p_c2).max()],
                               tickvals=[-1000000, -500000, 0, 500000, 1000000],
                               ticktext=['1M', '0.5M', '0', '0.5M', '1M'],
                               title='Number'),
                           barmode='overlay',
                           bargap=0.1)

        data = [go.Bar(y=y,
                       x=-p_c1,
                       orientation='h',
                       name=self.active_country,
                       hoverinfo='x',
                       marker=dict(color='powderblue')
                       ),
                go.Bar(y=y,
                       x=p_c2,
                       orientation='h',
                       name=self.reference_country,
                       text=-1 * p_c2.astype('int'),
                       hoverinfo='text',
                       marker=dict(color='seagreen')
                       )]


        fig = py.iplot(dict(data=data, layout=layout), filename='EXAMPLES/bar_pyramid')






    def compare_deathrates_demo(self):



        p_act = np.array(self.vulnerable_act)
        p_ref = np.array(self.vulnerable_ref)

        y = list(range(0, 100, 1))

        layout = go.Layout(yaxis=go.layout.YAxis(title='Age'),
                           xaxis=go.layout.XAxis(
                               range=[-max(list(p_act)+list(p_ref)), max(list(p_act)+list(p_ref))],
                               tickvals=[-50000, -25000, 0, 25000, 50000],
                               ticktext=["50k", "25k", "0", "25k", "50k"],
                               title='Number'),
                           barmode='overlay',
                           bargap=0.1)

        data = [go.Bar(y=y,
                       x=-p_act,
                       orientation='h',
                       name=self.active_country,
                       hoverinfo='x',
                       marker=dict(color='powderblue')
                       ),
                go.Bar(y=y,
                       x=p_ref,
                       orientation='h',
                       name=self.reference_country,
                       text=-1 * p_ref.astype('int'),
                       hoverinfo='text',
                       marker=dict(color='seagreen')
                       )]

        py.iplot(dict(data=data, layout=layout), filename='EXAMPLES/bar_pyramid')





    def show_corrected(self):


        ma = np.where(self.cases['Country/Region'] == self.active_country)[0]

        country_count = self.cases.iloc[ma, 4:].sum(axis=0)
        country_deaths = self.deaths.iloc[ma, 4:].sum(axis=0)

        plt.figure(figsize=(10,6))
        res1, = plt.plot(country_count[self.d_cases_act], 'bo-', label='Reported Cases ({})'.format(self.active_country))
        res2, = plt.plot(self.potential_cases, 'ro-', label='Adjusted Cases ({})'.format(self.active_country))

        plt.ylabel('cases')
        plt.xticks(rotation=90)
        plt.legend(handles=[res1, res2])
        plt.show()


