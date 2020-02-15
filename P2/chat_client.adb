with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Chat_Message;

procedure Chat_Client is

	package ATI renames Ada.Text_IO;
	package LLU renames Lower_Layer_UDP;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package CM  renames Chat_Message;

	Usage_Error: Exception;

	-- Comprobamos si lo que introducido por la linea de comando está bien
	procedure Check(Host: out ASU.Unbounded_String;
					Port: out Natural;
					Nick: out ASU.Unbounded_String) is
	begin

		if ACL.Argument_Count = 3 then
			Host := ASU.To_Unbounded_String(ACL.Argument(1));
			Port := Natural'Value(ACL.Argument(2));
			Nick := ASU.To_Unbounded_String(ACL.Argument(3));
		else
			raise Usage_Error;
		end if;

	end Check;

	--Envio de un mensaje Init al Servidor 
	procedure Send_Init(P_Buffer : access LLU.Buffer_Type;
						Client_EP: in LLU.End_Point_Type;
						Nick     : in ASU.Unbounded_String; 
						Server_EP: in LLU.End_Point_Type) is
	begin

		CM.Message_Type'Output(P_Buffer, CM.Init);
		LLU.End_Point_Type'Output(P_Buffer, Client_EP);
		ASU.Unbounded_String'Output(P_Buffer, Nick);
		LLU.Send(Server_EP, P_Buffer);
		LLU.Reset(P_Buffer.all);

	end Send_Init;

	--Envio de un mensaje writer al Servidor
	procedure Send_Writer(P_Buffer : access LLU.Buffer_Type;
						  Client_EP: in LLU.End_Point_Type;
						  Request  : in ASU.Unbounded_String; 
						  Server_EP: in LLU.End_Point_Type) is
	begin

		CM.Message_Type'Output(P_Buffer, CM.Writer);
		LLU.End_Point_Type'Output(P_Buffer, Client_EP);
		ASU.Unbounded_String'Output(P_Buffer, Request);
		LLU.Send(Server_EP, P_Buffer);
		LLU.Reset(P_Buffer.all);

	end Send_Writer;


	Request, Host, Nick, Reply: ASU.Unbounded_String;
	Server_EP, Client_EP: LLU.End_Point_Type;
	Buffer: aliased LLU.Buffer_Type(1024);
	Mess: CM.Message_Type;
	Port: Natural;

begin
	-- Construye el End_Point en el que está atado el servidor
	Check(Host, Port, Nick);
	Server_EP := LLU.Build(LLU.To_IP(ASU.To_String(Host)), Port);

	-- Construye un End_Point libre cualquiera y se ata a él
	LLU.Bind_Any(Client_EP);
	LLU.Reset(Buffer);
	Send_Init(Buffer'access, Client_EP, Nick, Server_EP);
	

	if ASU.To_String(Nick) /= "reader" then
		loop
			-- introduce el End_Point del cliente en el Buffer
			-- para que el servidor sepa dónde responder

			Ada.Text_IO.Put("Message: ");
			Request := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line);

			-- introduce el Unbounded_String en el Buffer
			-- (se coloca detrás del End_Point introducido antes)
			if ASU.To_String(Request) /= ".quit" then
				Send_Writer(Buffer'access, Client_EP, Request, Server_EP);
			else
				exit;
			end if;

		end loop;
	else
		loop
			LLU.Reset(Buffer);
			LLU.Receive(Client_EP, Buffer'Access);
			-- saca del Buffer un Unbounded_String
			Mess := CM.Message_Type'Input(Buffer'Access);
			Nick := ASU.Unbounded_String'Input(Buffer'Access);
			Reply := ASU.Unbounded_String'Input(Buffer'Access);
			ATI.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Reply));
		end loop;
	end if;
	-- termina Lower_Layer_UDP
	LLU.Finalize;

exception
	when Usage_Error | Constraint_Error =>
		ATI.Put_Line("Usage Error: ./chat_client <Host> <Port> <Nick>");
		LLU.Finalize;

	when Ex:others =>
		ATI.Put_Line ("Unexpected exception: " &
		Ada.Exceptions.Exception_Name(Ex) & " in: " &
		Ada.Exceptions.Exception_Message(Ex));
		LLU.Finalize;

end Chat_Client;
