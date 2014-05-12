#!/usr/local/bin/python

import sys
import string
import getopt
import math

"""
Script to calculate initial values for the phase mapping ROM.
Each bit indicates a rotational angle.
LSB indicates a 180 deg phase shift (1 = do the phase shift, 0 = do not)
All other bits indicate a clockwise rotation (1) or counterclockwise (0)
rotation of the x/y axes according to CORDIC values.
"""
def main(argv):
    # Local variables
    number_of_increments = 256
    number_of_cordic_shifts = 7
    
    # Process the arguments
    #  -i = number of increments
    #  -s = number of cordic shifts
    options, arguments = getopt.getopt(argv, "i:s:")
    for option, argument in options:
        if option=="-s":
            number_of_cordic_shifts = int(argument)
        elif option=="-i":
            number_of_increments = int(argument)
    
    # Call routine to calculate the phase shifts.
    phase_list = calculate_shifts(
        number_of_increments, number_of_cordic_shifts)
    
    # Print them to the standard output.
    for i in range(0, len(phase_list)):
        print(phase_list[i][1])
        
    
    
"""
Calculate strings for each of the phase shifts
"""
def calculate_shifts(number_of_increments, number_of_cordic_shifts):
    
    # For each of the phase values, calculate the string and add it to the list.
    phase_shifts = [45.0, 26.6, 14.0, 7.1, 3.6, 1.8, .9, .4]
    phase_list = list()
    
    # Check parameters and calculate the phase strings.
    if (number_of_cordic_shifts > 8) or (number_of_cordic_shifts < 1):
        # Numbe of shifts is out of range.
        print("Number of shifts must be in the range from 1 - 8.")
    elif (number_of_increments < 1):
        # Number of increments is out of range.
        print("Number of increments must be >= 1.")
    else:
        # Calculate the strings and add them to the list.
        for i in range(0, number_of_increments):
            # Phase increment ranges from -90 to 90
            # The LSB of the phase string indicates a 180 deg phase shift so
            # initialize to 0 (indicating no 180 deg phase shift)
            phase_increment = -90.0 + 180.0 * float(i)/ float(number_of_increments)
            phase_angle = phase_increment
            phase_string = "0"
    
            # If the current phase increment is greater than 0 rotate clockwise.
            # Otherwise, rotate counterclockwise.
            for j in range(0, number_of_cordic_shifts):
                if phase_increment > 0:
                    phase_string = "1" + phase_string
                    phase_increment -= phase_shifts[j]
                else:
                    phase_string = "0" + phase_string
                    phase_increment += phase_shifts[j]
    
            # Add the anagle and string to the list
            phase_list.append((phase_angle, phase_string))
    
    # Return
    return phase_list


"""
Main executable
"""
if __name__ == "__main__":
    main(sys.argv[1:])