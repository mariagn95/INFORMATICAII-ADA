with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Chat_Message;

procedure Chat_Admin is

	package ATI renames Ada.Text_IO;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package CM  renames Chat_Message;

	Usage_Error: exception;

	procedure Check(Host    : out ASU.Unbounded_String;
					Port    : out Natural;
					Password: out ASU.Unbounded_String) is
	begin

		if ACL.Argument_Count = 3 then
			Host := ASU.To_Unbounded_String(ACL.Argument(1));
			Port := Natural'Value(ACL.Argument(2));
			Password := ASU.To_Unbounded_String(ACL.Argument(3));
		else
			raise Usage_Error;
		end if;

	end Check;

	Server_EP, Admin_EP: LLU.End_Point_Type;
	Buffer: aliased LLU.Buffer_Type(1024);
	Request, Host, IP, Nick, Password_Admin, Data: ASU.Unbounded_String;
	Port: Natural;
	Mess: CM.Message_Type;
	Option: Integer := 0;
	Expired, Finish: Boolean;

begin

	Check(Host, Port, Password_Admin);
	IP := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Host)));
	Server_EP := LLU.Build(ASU.To_String(IP), Port);
	LLU.Bind_Any(Admin_EP);

	Finish := False;
	while not Finish loop
		ATI.Put_Line("Options:");
		ATI.Put_Line("1 Show writers collection");
		ATI.Put_Line("2 Ban writer");
		ATI.Put_Line("3 Shutdown server");
		ATI.Put_Line("4 Quit");
		ATI.Put("Your option? ");
		begin
			Option:= Integer'Value(ATI.Get_Line);
			LLU.Reset(Buffer);
			case Option is
				when 1 =>
					CM.Message_Type'Output(Buffer'Access, CM.Collection_Request);
					LLU.End_Point_Type'Output(Buffer'Access, Admin_EP);
					ASU.Unbounded_String'Output(Buffer'Access, Password_Admin);
					LLU.Send(Server_EP, Buffer'Access);
					LLU.Reset(Buffer);
					LLU.Receive(Admin_EP, Buffer'Access, 5.0, Expired);
					if Expired = True then
						ATI.Put_Line("Incorrect password");
					else
						Mess := CM.Message_Type'Input(Buffer'Access);
						Data := ASU.Unbounded_String'Input(Buffer'Access);
						ATI.Put_Line(ASU.To_String(Data));
					end if;

				when 2 =>
					ATI.Put("Nick to ban? ");
					Nick := ASU.To_Unbounded_String(ATI.Get_Line);
					CM.Message_Type'Output(Buffer'Access, CM.Ban);
					ASU.Unbounded_String'Output(Buffer'Access, Password_Admin);
					ASU.Unbounded_String'Output(Buffer'Access, Nick);
					LLU.Send(Server_EP, Buffer'Access);

				when 3 =>
					CM.Message_Type'Output(Buffer'Access, CM.Shutdown);
					ASU.Unbounded_String'Output(Buffer'Access, Password_Admin);
					LLU.Send(Server_EP, Buffer'Access);
					ATI.Put_Line("Server shutdown sent");

				when 4 =>
					LLU.Reset(Buffer);
					Finish := True;

				when others =>
					ATI.Put_Line("Incorrect option. Retry");
			end case;
		exception
			when Constraint_Error =>
				ATI.Put_Line("Incorrect option. Retry");
		end;
		ATI.New_Line(2);
	end loop;


	LLU.Finalize;

exception
	when Usage_Error | Constraint_Error =>
		ATI.Put_Line("Usage Error: ./chat_admin <Host> <Port> <Password>");
		LLU.Finalize;

	when Ex:others =>
		ATI.Put_Line ("Unexpected exception: " &
		Ada.Exceptions.Exception_Name(Ex) & " in: " &
		Ada.Exceptions.Exception_Message(Ex));
		LLU.Finalize;

end Chat_Admin;
