#!/usr/bin/env python3

import argparse
import os

def __parse_file(file_name):
    data = []
    with open(file_name, "r") as file:
        for line in file:
            data.append(line)
    return data

def __write_file(data, file_name):
    with open(file_name, "w") as file:
        file.writelines(data)
        file.flush()
    os.system("sync")

def __replace(data, constant, file):
    processed_data = []
    for line in data:
        # Check if a line contains an uneven number of single quotes
        if line.count("\'") % 2 != 0:
            #exit(1)
            print("WARNING! There is a uneven number of single quotes in this line of file {}. We leave this line as it is and don't process it".format(file))
            processed_data.append(line)
            continue

        # Process line
        second = -1
        while line[second +1: len(line)].find("\'") != -1:
            # Find first single quote
            first = line.find("\'", second+1)
            # Find second single quote
            second = line.find("\'", first+1)
            # Search for double quotes in between the first and second single quote and replace them with the constant
            line = line[:first] + line[first:second].replace("\"", constant) + line[second:]
        processed_data.append(line)
    return processed_data



def process(file_name, constant):
    """
    Replaces all double quotes which are wrapped in single code by a constant.\n
    Parameters
    ----------
    file_name : str\n
        File to be processed\n
    constant : str\n
        Constant by which double quotes in single quotes shall be replaced\n
    Returns
    ----------
    str\n
        Path to the processed file
    """
    #print("processing")
    data = __parse_file(file_name)
    #print(data)
    data = __replace(data, constant, file_name)
    #print(data)

    #__write_file(data, file_name + ".processed")
    __write_file(data, file_name)
    #return file_name + ".processed"


def unprocess(file_name,constant):
    # print('sed -i "s/{}/\\"/g" {}'.format(constant,file_name))
    os.system('sed -i "s/{}/\\"/g" {}'.format(constant,file_name))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="file to be processed")
    parser.add_argument("constant", help="constant by which double quotes in single quotes shall be replaced")
    parser.add_argument("--revert",action="store_true")
    args = parser.parse_args()
    #print("processing {}".format(args.file))
    if args.revert :
        unprocess(args.file, args.constant)
    else :
        process(args.file, args.constant)


if __name__ == "__main__":
    main()