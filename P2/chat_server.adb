with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Chat_Message;
with Client_Collections;

procedure Chat_Server is

	package ATI renames Ada.Text_IO;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package CM  renames Chat_Message;
	package CC  renames Client_Collections;

	Usage_Error, Shutdown: Exception;

	procedure Check(Port    : out Natural;
					Password: out ASU.Unbounded_String) is
	begin

		if ACL.Argument_Count = 2 then
			Port := Natural'Value(ACL.Argument(1));
			Password := ASU.To_Unbounded_String(ACL.Argument(2));
		else
			raise Usage_Error;
		end if;

	end Check;

	Server_EP, Client_EP, Admin_EP: LLU.End_Point_Type;
	Buffer: aliased LLU.Buffer_Type(1024);
	Request, Host, Nick, Password, Password_Admin, Data: ASU.Unbounded_String;
	Port: Natural;
	Mess: CM.Message_Type;
	Unique: Boolean;
	Writer, Reader: CC.Collection_Type;

begin

	-- construye un End_Point en una dirección y puerto concretos
	Host := ASU.To_Unbounded_String(LLU.Get_Host_Name);
	Check(Port, Password);
	ATI.Put_Line("I'm in: " & ASU.To_String(Host));
	ATI.Put_Line("Address: " & LLU.To_IP(ASU.To_String(Host)) & ":" & ACL.Argument(1));



	-- se ata al End_Point para poder recibir en él
	Server_EP:= LLU.Build(LLu.To_IP(ASU.TO_String(Host)), Port);
	LLU.Bind(Server_EP);

	-- bucle infinito
	loop
		-- reinicializa (vacía) el buffer para ahora recibir en él
		LLU.Reset(Buffer);
		LLU.Receive (Server_EP, Buffer'Access);
		-- saca
		Mess := CM.Message_Type'Input(Buffer'Access);
		case Mess is
			when CM.Init =>
				Client_EP := LLU.End_Point_Type'Input(Buffer'Access);
				Nick := ASU.Unbounded_String'Input(Buffer'Access);
				ATI.Put("INIT recieved from " & ASU.To_String(Nick));
				if ASU.To_String(Nick) /= "reader" then
					Unique := True;
					begin
						CC.Add_Client(Writer, Client_EP, Nick, Unique);
						LLU.Reset (Buffer);
						Request := ASU.To_Unbounded_String(ASU.To_String(Nick) & " joins the chat");
						CM.Message_Type'Output(Buffer'Access, CM.Server);
						ASU.Unbounded_String'Output(Buffer'Access, ASU.To_Unbounded_String("server"));
						ASU.Unbounded_String'Output(Buffer'Access, Request);
						CC.Send_To_All(Reader,Buffer'Access);
						ATI.New_Line;
					exception
						when CC.Client_Collection_Error =>
							ATI.Put_Line(". IGNORED, nick already used");
					end;
				else
					Unique := False;
					CC.Add_Client(Reader, Client_EP, Nick, Unique);
					ATI.New_Line;
				end if;

			when CM.Writer =>
				Client_EP := LLU.End_Point_Type'Input(Buffer'Access);
				Request := ASU.Unbounded_String'Input(Buffer'Access);
				ATI.Put("WRITER recieved from ");
				begin
					Nick := CC.Search_Client(Writer, Client_EP);
					ATI.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Request));
					LLU.Reset (Buffer);
					CM.Message_Type'Output(Buffer'Access, CM.Server);
					ASU.Unbounded_String'Output(Buffer'Access, Nick);
					ASU.Unbounded_String'Output(Buffer'Access, Request);
					CC.Send_To_All(Reader, Buffer'Access);
				exception
					when CC.Client_Collection_Error =>
						ATI.Put_Line("unknown client. IGNORED");
						LLU.Reset (Buffer);
				end;

			when CM.Collection_Request =>
				Admin_EP := LLU.End_Point_Type'Input(Buffer'Access);
				Password_Admin := ASU.Unbounded_String'Input(Buffer'Access);
				ATI.Put("COLLECTION_REQUEST received");
				if ASU.To_String(Password_Admin) = ASU.To_String(Password) then
					LLU.Reset (Buffer);
					Data := ASU.To_Unbounded_String(CC.Collection_Image(Writer));
					CM.Message_Type'Output(Buffer'Access, CM.Collection_Data);
					ASU.Unbounded_String'Output(Buffer'Access, Data);
					LLU.Send(Admin_EP, Buffer'Access);
					ATI.New_Line;
				else
					ATI.Put_Line(". IGNORED, incorrect password");
				end if; 

			when CM.Ban =>
				Password_Admin := ASU.Unbounded_String'Input(Buffer'Access);
				Nick := ASU.Unbounded_String'Input(Buffer'Access);
				ATI.Put("BAN received for " & ASU.To_String(Nick));
				if ASU.To_String(Password_Admin) = ASU.To_String(Password) then
					begin
						CC.Delete_Client(Writer, Nick);
						ATI.New_Line;
					exception
						when CC.Client_Collection_Error =>
							ATI.Put_Line(". IGNORED, nick not found");
					end;
				else
					ATI.Put_Line(". IGNORED, incorrect password");
				end if;

			when CM.Shutdown =>
				Password_Admin := ASU.Unbounded_String'Input(Buffer'Access);
				ATI.Put("SHUTDOWN received");
				if ASU.To_String(Password_Admin) = ASU.To_String(Password) then
					ATI.New_Line;
					raise Shutdown;
				else
					ATI.Put_Line(". IGNORED, incorrect password");
				end if;       
			when others =>
				null;
		end case;

		-- reinicializa (vacía) el buffer
		LLU.Reset (Buffer);

	end loop;

	-- nunca se alcanza este punto
	-- si se alcanzara, habría que llamar a LLU.Finalize;

exception
	when Shutdown =>
		LLU.Finalize;

	when Constraint_Error | Usage_Error =>
		ATI.Put_Line("Usage Error: ./chat_server <Port> <Password>");
		LLU.Finalize;

	when Ex:others =>
		Ada.Text_IO.Put_Line ("Unexpected exception: " &
		Ada.Exceptions.Exception_Name(Ex) & " in: " &
		Ada.Exceptions.Exception_Message(Ex));
		LLU.Finalize;

end Chat_Server;
