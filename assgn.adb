with ada.numerics.discrete_random;
with Ada.Text_IO, ada.Integer_Text_IO;
use Ada.Text_IO, ada.Integer_Text_IO;

package body Assgn is 
    package Rand_B is new ada.numerics.discrete_random(BINARY_NUMBER);
        use Rand_B;
    Procedure Init_Array(Arr: in out BINARY_ARRAY) is
        gen: Generator;
    begin
        Reset(gen);
        for i in Arr'Range loop
            Arr(i) := Random(gen);
        end loop;    
    end Init_Array;

    Procedure Print_Bin_Arr(Arr: in BINARY_ARRAY) is
    begin
        for i in Arr'Range loop 
            Put(Arr(i));
        end loop;
    end Print_Bin_Arr;

    Procedure Reverse_Bin_Arr(Arr: in out BINARY_ARRAY) is
        x : BINARY_NUMBER;
        i : INTEGER;
    begin
        i := 1;
        while i <= Arr'Length / 2 loop
            x := Arr(Arr'Length - i + 1);
            Arr(Arr'Length - i + 1) := Arr(i);
            Arr(i) := x;
            i := i + 1;
        end loop; 
    end Reverse_Bin_Arr;

    Function Int_To_Bin(Num : in INTEGER) return BINARY_ARRAY is
        op: BINARY_ARRAY;
        binary_value : INTEGER;
        val : INTEGER := Num;
    begin
        for i in reverse Binary_Array'range loop
            if val = 0 then
                binary_value := 0;
            else
                binary_value :=val mod 2;
                val :=val / 2;
            end if;
            output(i) := BINARY_NUMBER(binary_value);
        end loop;
        return output;
    end Int_To_Bin;

    Function Bin_To_Int(Arr: in BINARY_ARRAY) return INTEGER is
        x : INTEGER;
        op: INTEGER;
    begin 
        x := 1;
        op:= 0;
        for i in reverse Arr'range loop
            op:= op+ INTEGER(Arr(i)) * x;
            x := x * 2;
        end loop;        
        return output;
    end Bin_To_Int;

    function "+" (Left: in INTEGER; Right: in BINARY_ARRAY) return BINARY_ARRAY is
        op: BINARY_ARRAY;
        int_array: BINARY_ARRAY := Int_To_Bin(Left);
    begin
        op:= int_array + Right;
        return output;
    end "+";

    function "+" (Left, Right: in BINARY_ARRAY) return BINARY_ARRAY is
        tmp : INTEGER;
        op: BINARY_ARRAY;
    begin
        tmp := 0;
        for i in reverse Left'range loop
            tmp := tmp + Left(i);
            tmp := Right(i) + tmp;
            if tmp = 2 then
                output(i) := 0;
                tmp := 1;
            elsif tmp = 3 then
                output(i) := 1;
                tmp := 1;
            elsif tmp = 1 then
                output(i) := 1;
                tmp := 0;
            else 
                output(i) := 0;
                tmp := 0;
            end if;
        end loop;
        return output;
    end "+";

    Function "-"(Left: in INTEGER; Right: in BINARY_ARRAY) return BINARY_ARRAY is
        int_array: BINARY_ARRAY := Int_To_Bin(Left);
    begin
        return int_array - Right;
    end "-";

    Function "-" (Left: in BINARY_ARRAY; Right: in BINARY_ARRAY) return BINARY_ARRAY is
        tmp : INTEGER;
        carry : INTEGER := 0;
        op: BINARY_ARRAY;
    begin
        for i in reverse Left'range loop
            tmp := Left(i) - Right(i);
            if carry <= 0 then
                if tmp = 0 then
                    output(i) := BINARY_NUMBER(0);
                elsif tmp = 1 then
                    output(i) := BINARY_NUMBER(1);
                else
                    output(i) := BINARY_NUMBER(1);
                    carry := carry + 1;
                end if;
            else 
                if tmp = 0 then
                    output(i) := BINARY_NUMBER(1);
                elsif Left(i) = 1 and Right(i) = 0 then
                    output(i) := BINARY_NUMBER(0);
                    carry := carry - 1;
                else 
                    output(i) := BINARY_NUMBER(0);
                end if;    
            end if;
        end loop;
        return output;   
    end "-";

end Assgn;