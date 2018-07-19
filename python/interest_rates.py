#!/usr/bin/env python3
"""
Authors: Marcel Goh and Joanne Loh
18 July 2018

A collection of functions to calculate interest rates
"""

# given starting balance and monthly payment amount, returns end balance after given period
def end_bal(balance, annual_interest, payment, months):
    for _ in range(months):
        balance = balance + (annual_interest/12)*(balance-payment) - payment
    return balance

# returns total amount paid to interest over given period
def sum_interest(balance, annual_interest, payment, months):
    acc = 0
    for _ in range(months):
        acc += (annual_interest/12)*(balance-payment)
        balance = balance + (annual_interest/12)*(balance-payment) - payment
    return acc

# returns lowest monthly payment to pay off balance within given period
def lowest_payment(balance, annual_interest, months):
    low = balance/months
    high = balance * (1+annual_interest)**12/months
    payment = (low + high)/2.0
    while abs(end_bal(balance, annual_interest, payment, months)) > 0.01:
        if end_bal(balance, annual_interest, payment, months) > 0:
            low = payment
        else:
            high = payment
        payment = (low + high)/2.0
    return payment
