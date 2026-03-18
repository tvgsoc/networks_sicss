# SICSS Edinburgh 2025 - Social Network Analysis

This repository contains social network analysis teaching materials for the Summer Institutes in Computational Social Science in Edinburgh, 2025.

The first exercise uses data from a class network survey.  The main file is class_network.R. You'll need to download data files and adjust the working directory (or fork the whole repository, if you have Github installed)

We will look at networks of social media interaction on Twitter/X (retweet and secondarily mention networks). This is contained in the file mp_tweet_networks.R. This code is self-contained and loads data from another Github repository. You'll need to adjust the working directory for your system and create a subfolder "results" within that working directory.

Files related to Stochastic Actor-Oriented Models (SAOMs) were written by Jacky Wang for his undegraduate dissertation at the University of Edinburgh. If adapting these models please cite:

Wang, Jacky (2025). "Twitter Interaction Dynamics of UK MPs: A Longitudinal Social Network Analysis during the Brexit Negotiations." Undergraduate Dissertation, the University of Edinburgh.

Additionally, if time permits we will look at reply interactions on Reddit. This is contained in the file reddit_replies_network.R. Additionally, the file reddit_utterances_preprocessing.R illustrates pre-processing steps. However, running that file requires first downloading the Reddit corpus (small) from the convokit Python module. See here: https://convokit.cornell.edu/documentation/reddit-small.html


