# Options Backtester
# v0.1 requirements:
#    0. DONE be able to read in quotes from OptionVue
#       also, use quantmod to read the chains in from Yahoo finance
#    1. be able to tell the expiration months apart and keep track of DTE
#       a. what data structure to use here? read one matrix from disk at a 
#          time or store all data in an array or list of data frames?
#          put everything in one data frame with expirations caught in 
#          the symbol names? I think this is how it normally works.
#    2. be able to choose options based on greeks in the quotes
#    3. be able to track open P/L and closed P/L
#    4. be able to exit a trade on greeks in the quotes, P/L, DTE
#    5. be able to print the greeks of the total position every time interval