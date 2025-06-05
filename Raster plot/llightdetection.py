import pandas as pd 
import matplotlib.pyplot as plt

def process(filename, header=None, sep='\t'):
    df = pd.read_csv(filename, header=header, sep=sep, engine='python', error_bad_lines=False)
    
    spider_ids = [f's{i}' for i in range(1, 33)]
    colnames = ['index', 'date', 'time', 'status', 'extras', 'monitor', 'tube', 'dtype', '_', 'light'] + spider_ids
    df.columns = colnames

    df['datetime'] = pd.to_datetime(df['date'] + ' ' + df['time'], format="%d %b %y %H:%M:%S")
    for spider in spider_ids:
        df[spider] = (df[spider] > 0).astype(int)

    return df[['datetime', 'light'] + spider_ids] 

def lineplot(data, spiderid):
    spider_values = data[spiderid]
    maxval = 1 

    plt.figure(figsize=(12, 6))

    plt.fill_between(data['datetime'], 0, data['light'] * maxval, color="#ffff66", step="pre", alpha=0.5)

    plt.bar(data['datetime'], spider_values * maxval, width=0.01, color='black', align='center')

    plt.xlabel('Time')
    plt.ylabel('Activity (binarized)')
    plt.title(f'Binarized Activity Plot for {spiderid}')
    plt.tight_layout()
    plt.show()

if __name__ == "__main__":
    df = process("Monitor2.txt")
    lineplot(df, "s1")

df = pd.read_csv('Monitor2.txt', sep='\t', header=None)
df['datetime'] = pd.to_datetime(df[2] + ' ' + df[3], format='%d %b %y %H:%M:%S')
df.set_index('datetime', inplace=True)
df.drop(columns=[1, 5, 7, 8, 9], inplace=True)
df.head()