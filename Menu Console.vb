
Imports System.Math ' allows importing of the math equations without writing out equations fully.
Imports System.Text
Imports System
Module Module1
    Sub Main()
        Call Menu()  ' calls Menu sub the main sub that holds the menu interface and carrys the return values for certain sections.
    End Sub

    Sub Menu() ' THIS IS THE CORE PRIME MENU AREA
        On Error GoTo Error_MSG ' Goes to error message if the console displays a error code the error code will be displayed by line 46
        Console.Title = ("-----------------------------------------------CS0002 Jatinders (1945979) COURSEWORK------------------------------------------------")
        Dim decimals As Integer = 3   ' linked to the accuracy option default value for decimal places is '3' so in theory this will be overwritten for whatever the user has chosen if not for some reason it will stick to '3' decimal places
        Dim input As String ' Declaring input as string 
        Dim RunningProgram As Boolean  'Including while loop so the user can input the numbered options from 1-6 if false it will not read the input of the user and will stay blank if changed to "false"
        RunningProgram = True
Menu_Start:
        While RunningProgram = True  ' While loop in order to return to menu process again when user leaves sub sections
            'Console.Clear()
            Console.WriteLine("MENU OPTIONS")     'Displays the text written in vb to the console window of which is the menu interface shown to the user.
            Console.WriteLine("=========================================================")
            Console.WriteLine("Options between 1 and 6 are below:")
            Console.WriteLine("1: Accuracy Option")
            Console.WriteLine("2: Quadratic Equation")
            Console.WriteLine("3: Protein Sequence Segmentation")
            Console.WriteLine("4: Prime numbers")
            Console.WriteLine("5: Help")
            Console.WriteLine("6: Exit")
            Console.WriteLine("Please enter your choice:")
            input = Console.ReadLine()  ' Reads input of user when the user types on the menu interface 
            If input = 1 Then  'input option1
                decimals = AccuracyOption() 'Decimals is Accuracy option it should override the default decimal place of 2
            ElseIf input = 2 Then
                Call QuadraticEquation(decimals) ' Formats the result in the quadratic equation to 2 decimal places based on input.
            ElseIf input = 3 Then
                Call SequenceSegementation()
            ElseIf input = 4 Then
                Call Primenumber(decimals)   ' Formats the decimal places based on choice of user within Accuracy option. Formatting the time to how many decimal places the user chooses.
            ElseIf input = 5 Then
                Call Help()
            ElseIf input = 6 Then
                Call Exitconsole()   ' Calls the exitconsole subroutine to exit the program.
            ElseIf input <> 1 Or 2 Or 3 Or 4 Or 5 Or 6 Then ' if options are not = from 1 to 6 then it will go to error message
                GoTo Error_MSG ' Validation steps if numbers 1-6 are not inputted by the user it will display a message until the numbers 1-6 have been inputted.
Error_MSG:
                Console.WriteLine("Input INVALID please choose the numbered options above.")
                Console.WriteLine("") ' Creates a space before redisplaying menu
                GoTo Menu_Start ' goes to the start of the program and what is shown
            End If
        End While

    End Sub
    Function AccuracyOption() ' Function instead of subroutine to return value 
        Console.Clear()
        Console.WriteLine("You have now selected option: 1 and are in the ACCURACY OPTION:")
        Console.WriteLine("=========================================================================")
        Dim decimals1 As Integer  ' Declared decimals1 as integer 
Accuracystart:
        Console.WriteLine("Please select a option between 1-5 decimal places:")
        decimals1 = Console.ReadLine()
        If decimals1 > 0 And decimals1 <= 5 Then  'Conditions to include bigger than '0' so '1' is included and smaller but = to '5' so 5 as itself can be chosen
            Console.WriteLine("You have selected " & decimals1 & " decimal places")
        Else
            Console.WriteLine("Invalid input please try again:")  ' validation message if numbers are not 1 to 5
            GoTo Accuracystart     ' Loops back to select option 1 to 5
        End If
        Console.WriteLine("================================")
        Call Submenu3()   ' calls the subroutine menu to call 2 options: 1 called "Option", 2 called "Exit". so if you press "0" it will redisplay the menu and you can choose the menu or press "6" and it will exit the program.
        Return decimals1  ' Returns value of what the user inputted!
    End Function
    Sub QuadraticEquation(decimals As Integer) ' allows the users choice of decimal places to be carried to this subroutine.
        Console.Clear()
        Console.WriteLine("You have now selected option: 2 and are in the QUADRATIC EQUATION OPTION:")
        Console.WriteLine("=========================================================================")
        Console.WriteLine("Welcome To the Quadratic Equation calculator, Input values as instructed below to get a answer:")
        Dim a, b, c, s1, s2 As Double ' A,B,C is part of the main variables sqrt1 and sqrt2 will link to the result
        Dim sqrt1, sqrt2, result1, result2 As Double
        Console.WriteLine("FORMULA:  -b + or - Math.Sqrt(b ^ 2 - 4 * a * c)) / 2 * a")
        Console.WriteLine("--------------------")
Line1:  Console.WriteLine("Enter value of a:")
        a = Console.ReadLine()
        If a = 0 Then
            Console.WriteLine("Invalid input 0 DOES NOT PROVIDE A VIABLE SOLUTION!!!!") ' Entering 0 will give no value but a error that is still calculated.
            GoTo Line1  ' if a is equal to 0 then it will loop back to the label "line1" to repeat until the user doesn't enter 0
        End If
        Console.WriteLine("--------------------")
        Console.WriteLine("Enter value of b:") 'text that is going to be displayed on console window
        b = Console.ReadLine()   'reads the input value of 'b'
        Console.WriteLine("--------------------")
        Console.WriteLine("Enter value of c:")
        c = Console.ReadLine()
        sqrt1 = (-b + Math.Sqrt(Math.Abs(b ^ 2 - 4 * a * c / 2 * a))) ' Quadratic formula usually 2 answers are shown one positive and one negative
        sqrt2 = (b - Math.Sqrt(Math.Abs(b ^ 2 - 4 * a * c / 2 * a)))
        result1 = (-b + sqrt1) / (2 * a)
        result2 = (+b - sqrt2) / (2 * a)
        '====================================================================================================='
        Dim complexroot1 As Double ' Declaring complexroot1, complexroot2 as double
        Dim complexroot2 As Double
        s2 = Math.Pow(b, 2) - (4 * a * c) 'Math.pow is taking the power of the root by giving a accurate number that is not too high and not to low only working out the complex root part of ax^2+bx=c and s2 is getting the value in the square root
        If s2 > 0 Then
            s1 = Math.Sqrt(s2)
            complexroot1 = (-b + s2) / (2 * a) 'checking for equal and complex  roots  
            complexroot2 = (-b - s2) / (2 * a)
        ElseIf s2 = 0 Then
            complexroot1 = -b / 2
            complexroot2 = complexroot1
            'If quadratic has complex roots:
        Else s1 = Math.Round(Math.Sqrt(-1 * s2) / decimals, decimals) ' Value of s1 is rounded then square rooted and the decimals are relating to the formating output of the decimal places.
            complexroot1 = -b / 2
        End If

        Console.WriteLine("================================")
        Console.WriteLine("The solution for result1 of which is (negative) is..." & Roundingnumber(result1, decimals))  'Rounding= "Rounds result input", Result1= "Answer to the quadratic inputs", Decimals is the amount of decimals places based on the users option. 
        Console.WriteLine("The solution for result2 of which is (positive) is..." & Roundingnumber(result2, decimals))
        Console.WriteLine("The solution with complex root for" & "[ " & Roundingnumber(result1, decimals) & " ]" & " of which is root..." & (" - i" & s1 & " the number may of been rounded")) ' Displays users answer with complex root
        Console.WriteLine("The solution with complex root for" & "[ " & Roundingnumber(result2, decimals) & " ]" & " of which is root..." & (" + i" & s1 & " the number may of been rounded")) ' Display ysers asnwer with complex root
        Console.WriteLine("================================")
        Call Submenu3() ' calls the subroutine menu to call 2 options: 1 called "Option", 2 called "Exit". so if you press "0" it will redisplay the menu and you can choose the menu or press "6" and it will exit the program.
    End Sub
    Sub SequenceSegementation()
        Console.Clear()
        Console.WriteLine("You have now selected option: 3 and are in the SEQUENCE SEGEMENTATION OPTION:")
        Console.WriteLine("=========================================================================")
            Const sequencefile = "D:\Visual Studio code 2019 projects\Hello world project\C0002 ASSESSMENT BRIEF\C0002 coursework\C0002 coursework\bin\Debug\proteinsequence.txt"
        Using fileReader As New IO.StreamReader(sequencefile)
            Console.WriteLine("Sequence read from file.........................\-\-\")
            Dim fileone = fileReader.ReadLine 'Reads the protein  sequence within the file.
            Console.WriteLine(fileone)
            Console.WriteLine("=====================================================")
            Console.WriteLine("The Modified sequence...........................\-\-\")
            Console.WriteLine("=====================================================")
            Dim character_length = Len(fileone)   'Length of the protein sequence within the file
            Dim LetterArray() As Char = fileone.ToArray
            fileReader.Close()  ' file has been closed.
            Dim begin As Long = 0
            Dim ending As Long
            For a = 0 To character_length - 2
                If (LetterArray(a) = "R" And LetterArray(a + 1) <> "P") Or (LetterArray(a) = "K" And LetterArray(a + 1) <> "P") Then 'Seperates the protein sequence and cuts the code has certain variables within the sequence.
                    ending = a
                    For b = begin To ending
                        Console.Write(LetterArray(b)) '
                    Next b
                    begin = ending + 1
                    Console.WriteLine()
                End If
            Next a
            For c = ending + 1 To character_length - 1 ' It will take all characters from 'c' ending to the length or the variable in this case 'character length' so it can run the code for each value.
                Console.Write(LetterArray(c))
            Next c
        End Using
        Console.WriteLine()
        Console.WriteLine("================================")
        Call Submenu3() ' goes to subroutine involving 2 option interfaces as a mini-menu
    End Sub
    Function Primenumber(decimals As Integer)
        On Error GoTo Validation_1 'If a error occurs it will go to the label to display the message and restart the process again.
        Console.Clear() ' Clears previous input of the user so it will not save it making the program look messy and confusing.
        Console.WriteLine("You have now selected option: 4 and are in the PRIME NUMBER OPTION:")
        Console.WriteLine("=========================================================================")
        Dim num01, num02, num03 As Long ' My 3 variables num01,num02,num03 as long not integer. 
        Dim PrimestartMilliseconds As Double = Now.Millisecond
        Dim PrimeStartSeconds As Double = Now.Second
        Dim PrimestartMinutes As Double = Now.Minute
Start_1:
        Console.WriteLine("Input a prime number")
        num01 = Console.ReadLine()      'reads the input of the user
        Console.WriteLine("------------------------------")
        Console.WriteLine("THE NUMBER INPUTTED is " & num01) ' displays the text and input of the user
        Console.WriteLine("------------------------------")
        num03 = 1 ' Check the prime number 
        For num02 = 2 To (num01 - 1)
            If num01 Mod num02 = 0 Then
                num03 = 0 ' checks the number '1' against 0
            ElseIf num01 Mod num02 = 0 Then 'Once calculated num01 and num02 the output determines if it's prime or not depending if it meets the condition '=0'
                GoTo Primenumber_evaluation ' Goes to the label to state whether it's prime or not.
                Exit For
            End If
        Next
Primenumber_evaluation:
        If num03 = 0 Then
            Console.WriteLine(num01 & " is not a prime Number") ' If variable 'num01' which is users input is not a prime it will go to another label finish_1 where it would calculate the time it took.
            GoTo finish_1 ' goes to the timer section of the code
        Else
            Console.WriteLine(num01 & " is a prime Number")
            GoTo finish_1
        End If
Validation_1: ' if validation console message is displayed and then goes to Start_1 line 169 to restart the process again.
        Console.WriteLine("Invalid INPUT please try again system cannot handle above !")
        GoTo Start_1
finish_1:
        Console.WriteLine("------------------------------")
        Console.WriteLine("The time taken to calculate the value inputted:")
        Dim PrimeEndMilliseconds As Double = Now.Millisecond
        Dim PrimeEndSeconds As Double = Now.Second
        Dim PrimeEndMinutes As Double = Now.Minute
        Dim Primestart_total As Double = (PrimestartMinutes * 60) + (PrimeStartSeconds * 1000) + (PrimestartMilliseconds)
        Dim PrimeEnd_total As Double = (PrimeEndMinutes * 60) + (PrimeEndSeconds * 1000) + (PrimeEndMilliseconds)
        Dim Prime_Difference As Double = PrimeEnd_total - Primestart_total
        If Prime_Difference > 1000 Then
            Prime_Difference = Prime_Difference / 1000
            Console.WriteLine("It took " & Roundingnumber(Prime_Difference, decimals) & " seconds") 'Uses rounded function for decimal places and displays the amount of seconds and decimal places of which the user has chosen.
        Else
            Console.WriteLine("It took " & Roundingnumber(Prime_Difference, decimals) & " milliseconds")
        End If
        Console.WriteLine("================================")
        Call Submenu3()
    End Function
    Sub Help()
        Console.Clear() ' Clears previous input answer
        Console.WriteLine("You have now selected option:  5 and are in the HELP OPTION:")
        Console.WriteLine("=========================================================================")
        Console.WriteLine()
        Console.WriteLine("HOW DOES THIS PROGRAM WORK?.......................\-\-\")
        Console.WriteLine("This program works as a menu interface, as the user of which is yourself you must choose from the option numbers which are 1,2,3,4,5,6. Each number is a option.")
        Console.WriteLine("To return the menu when you have accessed a option press'0'.")
        Console.WriteLine("To exit the menu when you have accessed a option press'6'.")
        Console.WriteLine()
        Console.WriteLine("=============================ISSUES AND REPORTING=========================")
        Console.WriteLine("If any bugs, or issues occur please contact the adminstrator.")
        Console.WriteLine("==========================================================================")
        Call Submenu3() ' Links to the small menu interface within each subroutine to go back to menu or exit the program by pressing "0" is menu and by pressing "6" is exiting the program.
    End Sub
    Sub Exitconsole()  ' Links to the main menu  if option 6 is pressed will go to exitconsole()
        Console.Clear() '
        Console.WriteLine("You have now exited the application!")
        Dim running6 As Integer 'Declaring as number 
        running6 = 6        ' Setting integer value as '6'
        Environment.Exit(running6)   ' Program is exited if numeber 6 is pressed.
        Console.ReadLine()
    End Sub
    '===========================================================SUBROUTINE INTERFACE TO RETURN TO MENU OR EXIT THE PROGRAM!==============================
    '====================================================================================================================================================
    Sub Submenu3() ' For the returning back to menu option
        On Error GoTo Error_Msg ' If error occurs the system will go to the label Error_MSG
        Console.WriteLine("Press 0 to return to the main menu")  ' Text to display in each subroutine of the menu
        Console.WriteLine("0: Return to Menu")                   ' Tells the user in the interface which option to press
        Console.WriteLine("Press 6 to exit the application")
        Console.WriteLine("6: Exit")
        On Error GoTo Returnstart
        Dim input As Integer    ' Declaring my input as number

Returnstart:
        input = Console.ReadLine() ' reads input of the user
        If input = 6 Then
            Call Exitconsole()   ' calls exitconsole sub- routine and exits the console entirely if the user presses '6'
        ElseIf input = 0 Then
            GoTo Finish
        ElseIf input <> 0 Or 6 Then ' Error Message is also displayed when input is not equal to 0 or 6
Error_Msg:
            Console.WriteLine("Input INVALID please choose '0' to return to menu and please choose '6' to exit numbered options above:")
            GoTo Returnstart 'Once message is displayed loops back to the return start
        End If
Finish:
    End Sub
    Sub Submenu2()  ' links to the code above so when 0 is inputted returns to menu
        Console.Clear()
        Dim returning As Integer ' returning is declared as a number
        Dim returning2 As Boolean ' declared as boolean 
        returning2 = True  'for the while loop is = to true then it can run the code within it.
        While returning2      ' If the loop is true it will run the code within the loop if the loop is false it won't run the code.
            If returning = 0 Then ' Returns to main menu when 0 is inputted
                Call Menu()      ' Returns to main menu.
            End If
            Return
        End While
    End Sub
    '====================================================================================================================================================
    '====================================================================================================================================================
    Function Roundingnumber(Input As Double, rounding1 As Double) ' includes decimal numbers
        If rounding1 = 1 Then  'if user picks input 1 then the formatting of the answer will be in 1 decimal place
            Return Format(Input, "0.0")
        ElseIf rounding1 = 2 Then
            Return Format(Input, "0.00")
        ElseIf rounding1 = 3 Then
            Return Format(Input, "0.000")
        ElseIf rounding1 = 4 Then
            Return Format(Input, "0.0000")
        ElseIf rounding1 = 5 Then
            Return Format(Input, "0.00000")
        Else
            Return Input  ' returns value of input depending on what the user has chosen.
        End If
    End Function
End Module



