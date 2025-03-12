
	// Encodes given string into given BYTE array using given encoding.
	// If target array is too small, given string will be cut. If target array is too large, remaining bytes will be set to NULL (0).
	FUNCTION ENCODE_STRING : VOID
		VAR_INPUT
			in : STRING[254];
			encoding : STRING_ENCODING;
		END_VAR
		
		VAR_IN_OUT
			out : ARRAY[..] OF BYTE;
		END_VAR
		
		VAR_TEMP
			in_len : INT;
			out_len : DINT;
			out_start : DINT;
			out_end : DINT;
			i : DINT;
		END_VAR
		
		in_len := LEN(in);
		
		out_start := _firstIndexOf(out);
		out_end := _lastIndexOf(out);
		out_len := out_end - out_start + 1;
		
		FOR i := 1 TO out_len DO
			IF i > in_len THEN
				out[out_start + i - 1] := 0; // NULL
			ELSIF encoding = ASCII AND in[i] > 127 THEN
				out[out_start + i - 1] := 63; // Questionmark '?'
			ELSE	
				out[out_start + i - 1] := in[i];
			END_IF;
		END_FOR;
	END_FUNCTION
	