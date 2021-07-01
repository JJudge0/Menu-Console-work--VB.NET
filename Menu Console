Imports System.Text 'allows placing import statements at the top of the program.
Imports System.Math ' allows importing of the math equations without writing out equations fully.
Module Module1
    Sub Main() 'Name: JJ
        Console.WriteLine("5.2 RECRUSIVE FUNCTIONS----------------------------------------------------------")
        'Console.WriteLine("What is a Recrusive function?")
        'Console.WriteLine("A recursive function is a function that calls itself during its execution. This enables the function to repeat itself several times, outputting the result and the end of each iteration")

        Console.WriteLine("5.2.1 FACTORIAL----------------------------------------------------------")
        'Console.WriteLine("TASK: Develop a function that returns x! for a given number x, note that this requires a very small change to be made to the recursive function we discussed in the lecture. Make sure you test for all of the valid ranges for x. What is the maximum value of x! that VB can handle?"
        Console.WriteLine("Factorial of number below")
        Console.WriteLine(Fact(5)) 'Change number "5" to get the factorial of any number.
        ' Console.WriteLine("TO GET FACTORIAL USED RECRUSION METHOD")
        'Console.Writeline(" Factorial is a number of which all the previous numbers like 5 is 1,2,3,4,5 = 1X2X3X4X5=120

        Console.WriteLine("5.2.2 FIBONACCI NUMBERS----------------------------------------------------------")
        Console.WriteLine("We looked at the Fibonacci numbers in a previous exercise. Write a recursive version that returns the nth Fibonacci number")
        Dim n As Integer '"n" in this case is the nth term.
        n = Console.ReadLine()
        Console.WriteLine()
        Console.WriteLine("The fibonacci number " & n & " value is:")
        Console.WriteLine(fib(n))
        Console.WriteLine()

        Console.WriteLine("5.2.3 THE MCCARTHY 91 FUNCTION----------------------------------------------------------")
        Console.WriteLine("So the MCcarthy sequence is where the McCarthy-91 function is the recursive function defined for positive integer n by (FORMULA IN THE LAB SHEET) It takes the value 91 for all n=1, 2, ..., 101, and then continues 92, 93, 94.")
        Dim M1 As Integer
        M1 = 101
        Console.WriteLine((TMC91(M1)))
        'Console.WriteLine("Task: Test the function on integers ranging from -250 to 250. What do you notice about the function?")
        'Console.WriteLine("ANSWER: When putting the integers ranging from negative -250 to 250 the numbers are not 91 they increase in negative and decrease in the positive. ")

        Console.WriteLine("5.2.4 PALINDROME----------------------------------------------------------")
        Console.WriteLine("What is Palidrone? TASK BELOW")
        Console.WriteLine("A palindrome is a word, phrase, verse, or sentence that reads the same backwards or forwards. Write a recursive function that takes as input a character string and returns true or false depending on whether the string is a palindrome or not. Look on the web for some useful test cases (search for palindrome on Google for example).")
        'USE EXAMPLES FOR PALIDROME LIKE "jatinder" THE OUTCOME SHOULD SAY "The word above is not Palindrome" AND ANOTHER WORD "rotor" which should say "The word above is Palindrome". IT DOESN'T ACCEPT CAPITAL LETTERS AND WILL SAY "The word above is not Palindrome"
        Dim pal As String
        Console.WriteLine("Please enter a word that might be palindrome:")
        pal = Console.ReadLine()
        Console.WriteLine("Is the word provided a palindrome?")
        Console.WriteLine(palin(pal)) ' Checks if the word is read left to right and right to left.
        Console.WriteLine()

        Console.WriteLine("5.2.5 CHARACTER COUNTER----------------------------------------------------------")
        Console.WriteLine("Write a recursive function that returns the number of times that a character occurs in a string. For example, if the string is 'abracadabra' and the character is ‘a’ then the function returns 5.")

        Dim Num1, num2 As String
        Num1 = Console.ReadLine()
        Console.WriteLine()
        Console.WriteLine("Enter a letter to search for")
        num2 = Console.ReadLine()
        Console.WriteLine("RESULT OF SEARCH:" & "---------------" & "WORD SEARCHED:" & Num1 & "," & Charactercount(Num1, num2))
        Dim E1 As Integer
        Console.WriteLine(Test(E1))
    End Sub
   -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    Function Fact(ByVal n As Integer)
        If (n = 0) Then
            Fact = 1
        Else
            Fact = n * Fact(n - 1)
        End If
    End Function
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
    Function fib(nthterm As Integer)
        If nthterm = 0 Then
            Return 0
        ElseIf nthterm = 1 Then
            Return 1
        End If
        Return (fib(nthterm - 1) + fib(nthterm - 2))
    End Function
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
    Function TMC91(M1 As Integer)
        If M1 < 1 Then
            Return (M1 - 100) ' All numbers from 1 to 100 have the return function of 91 anything above it will increase.

        ElseIf M1 <= 100 Then
            TMC91 = TMC91(TMC91(M1 + 11))
        Else
            TMC91 = M1 - 10
        End If
        Return TMC91
        Exit Function
    End Function 
    -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    Function palin(pal As String)
        Dim R1, T1, F1 As String
        T1 = "The word above is Palindrome"
        F1 = "The word above is not Palindrome"
        R1 = StrReverse(pal) ' reverses string  of whatever word has been inputted.
        If pal = R1 Then Return T1
        If pal <> R1 Then
            Return F1
        End If
        Return pal  'returns value
        Exit Function
    End Function
       -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    Function Charactercount(ByVal word As String, ByVal num5 As Char) As Integer
        Dim num4 As Integer = 0
        For Each c As Char In word
            If c = num5 Then
                num4 += 1
            End If
        Next
        Return num4 'counts the amount of characters once specified the certain word chosen
    End Function
     -----------------------------------------------------------------------------------------------------------------------------------------------------------------
    Function Test(ByVal E1 As Integer) As Long
        Dim d, f As Integer
        d = 2
        f = 10
        E1 = d + f
        Return E1
    End Function
   -----------------------------------------------------------------------------------------------------------------------------------------------------------------
End Module






