
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    DISTANCIA_SEGURA: constant Integer := 0;
    DISTANCIA_INSEGURA: constant Integer := 1;
    DISTANCIA_IMPRUDENTE: constant Integer := 2;
    PELIGRO_COLISION: constant Integer := 3;
    M1: constant Integer := 1;
    M2: constant Integer := 2;
    M3: constant Integer := 3;

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
         --Put_Line("BACKGROUND");
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------
   
    -- 0.15
    task Riesgos is 
    pragma priority(5);
    end Riesgos;
    
    -- 0.3
    task Distancia is
    pragma priority(4);
    end Distancia;
    
    -- 0.35
    task Giros is
    pragma priority(3);
    end Giros;
   
    -- 0.4
    task Cabeza is 
    pragma priority(2);
    end Cabeza;

   -- 1
    task Display is
    pragma priority(1);
    end Display;

    ---------------------------------------------------------
    -- Aqui se declaran las tareas que forman el STR
    ---------------------------------------------------------
    ----------------------------------------------------------------------
    ------------- declaracion del tipo de objeto protegido 
    ----------------------------------------------------------------------
    
    protected type Sintomas(valor_ini: integer) is 
    	pragma priority(10);
    	procedure escribir_cabeza_sint(nuevo_valor_bool: boolean);
      procedure escribir_dist_sintoma(nuevo_valor: integer); 
      procedure escribir_sint_vol(nuevo_valor: boolean);
      function leer_cabeza_sin return boolean;
      function leer_dist_sin return integer;
      function leer_vol_sin return boolean;
    private 
    	sint_dist: integer := valor_ini;
    	sint_cabeza: boolean := false;
      sint_vol: boolean := false;
    end Sintomas;
   
    protected type Medidas is
    	pragma priority(11);
    	procedure escribir_dist_vel(dist: Distance_Samples_Type; vel: Speed_Samples_Type);
      function leer_distancia return Distance_Samples_Type;
      function leer_velocidad return Speed_Samples_Type;
    private
    	velocidad: Speed_Samples_Type;
    	distancia: Distance_Samples_Type;
    end Medidas;

    protected type InterruptHandler(valor_ini: integer) is
      pragma priority(12);
      procedure siguiente_modo;
      function leer_modo return integer;
    private
      modo: Integer := valor_ini;
    end InterruptHandler;

    sint: Sintomas(0);
    medida: Medidas;
    interrupt_handler: InterruptHandler(1);

    ----------------------------------------------------------------------
    ------------- cuerpo de objeto protegido 
    ---------------------------------------------------------------------- 
     
    protected body Sintomas is
    	function leer_cabeza_sin return boolean is
    	begin
    		return sint_cabeza;
    	end leer_cabeza_sin;
    	function leer_dist_sin return integer is
    	begin
    		return sint_dist;
    	end leer_dist_sin;
      function leer_vol_sin return boolean is
    	begin
    		return sint_vol;
    	end leer_vol_sin;
    	procedure escribir_cabeza_sint(nuevo_valor_bool: boolean) is 
    	begin 
    	sint_cabeza := nuevo_valor_bool;
    	end escribir_cabeza_sint;

      procedure escribir_dist_sintoma(nuevo_valor: integer) is 
    	begin 
    	sint_dist := nuevo_valor;
    	end escribir_dist_sintoma;

      procedure escribir_sint_vol(nuevo_valor: boolean) is
      begin
         sint_vol := nuevo_valor;
      end escribir_sint_vol;
    end Sintomas;


    protected body Medidas is
      function leer_distancia return Distance_Samples_Type is 
    	begin 
    	   return distancia;
    	end leer_distancia;
      function leer_velocidad return Speed_Samples_Type is 
    	begin 
    	   return velocidad;
    	end leer_velocidad;
    	procedure escribir_dist_vel(dist: Distance_Samples_Type; vel: Speed_Samples_Type) is 
    	begin 
         velocidad := vel;
         distancia := dist;
    	end escribir_dist_vel;
    end Medidas;

    protected body InterruptHandler is
      function leer_modo return Integer is
      begin
         return modo;
      end leer_modo;
      procedure siguiente_modo is
      begin
         if modo = M1 then
            modo := M2;
         elsif modo = M2 and sint.leer_dist_sin /= PELIGRO_COLISION and sint.leer_cabeza_sin = false then
            modo := M3;
         elsif modo = M3 then
            modo := M1;
         end if;
      end siguiente_modo;
    end InterruptHandler;
    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------

   task body Cabeza is
      t_sig : Time;
      intervalo : Time_Span := To_Time_Span(0.4);
      cabeza_act: HeadPosition_Samples_Type := (0,0);
      cabeza_ant: HeadPosition_Samples_Type := (0,0);
      sw_act: Steering_Samples_Type := (0);
      sw_ant: Steering_Samples_Type := (0);
      begin
      t_sig := Big_Bang + intervalo;
      loop
         Starting_Notice("COMENZANDO TAREA CABEZA");
         Reading_HeadPosition (cabeza_act);
         Display_HeadPosition_Sample (cabeza_act);
         Reading_Steering (sw_act);
         Display_Steering (sw_act);

         if(((cabeza_act(x) > 30) and (cabeza_ant(x) > 30)) or
            ((cabeza_act(x) < -30) and (cabeza_ant(x) < -30)) or
            (((cabeza_act(y) > 30) and (cabeza_ant(y) > 30)) and ((sw_act < 5 and sw_ant < 5))) or
            (((cabeza_act(y) < -30) and (cabeza_ant(y) < -30)) and ((sw_act > -5 and sw_ant > -5))))
         then 
            sint.escribir_cabeza_sint(true);
         else 
            sint.escribir_cabeza_sint(false);
         end if;
         cabeza_ant := cabeza_act;
         sw_ant := sw_act;
         Finishing_Notice("FIN TAREA CABEZA");
         delay until t_sig;
         t_sig := t_sig + intervalo;
      end loop;
   end Cabeza;


   task body Distancia is 
    
   dist_act: Distance_Samples_Type := 0;
   vel_act: Speed_Samples_Type := 0;
   cal_vel: Speed_Samples_Type := 0;
   sig_instante: Time;
   intervalo: Time_Span := To_Time_Span(0.3);
    
   begin

      sig_instante := Big_Bang + intervalo;

      loop
    	   Starting_Notice("COMIENZA TAREA DISTANCIA");
         Reading_Distance (dist_act);
    	   Reading_Speed (vel_act);
    	
    	   medida.escribir_dist_vel(dist_act, vel_act);

         cal_vel := (vel_act/10)**2;
    	
    	   if (Float(dist_act) < Float(cal_vel) )then
    		   sint.escribir_dist_sintoma(DISTANCIA_INSEGURA);
    	   elsif (Float(dist_act) < Float(cal_vel/2) )then
        	   sint.escribir_dist_sintoma(DISTANCIA_IMPRUDENTE); 
         elsif(Float(dist_act) < Float(cal_vel/3) )then
        	   sint.escribir_dist_sintoma(PELIGRO_COLISION);
         else  sint.escribir_dist_sintoma(DISTANCIA_SEGURA);
         end if;

         Finishing_Notice("FIN TAREA DISTANCIA");
         delay until sig_instante;
         sig_instante := sig_instante + intervalo;
      end loop;

   end Distancia;

   task body Display is
      dist_act: Distance_Samples_Type := 0;
      vel_act: Speed_Samples_Type := 0;
      cabeza_act: HeadPosition_Samples_Type := (0,0);
      vol_act: Steering_Samples_Type := 0;
      dis_sin_act: integer := 0;
      sig_instante: Time;
      intervalo: Time_Span := To_Time_Span(1.0);

   begin
      sig_instante := Big_Bang + intervalo;

      loop
         Starting_Notice("COMIENZA TAREA DISPLAY");

         dist_act := medida.leer_distancia;
         vel_act := medida.leer_velocidad;
         Display_Distance(dist_act);
         Display_Speed(vel_act);
         New_Line;

         dis_sin_act := sint.leer_dist_sin;
         if (dis_sin_act /= DISTANCIA_SEGURA) then
            if (dis_sin_act = DISTANCIA_IMPRUDENTE) then
               Put_Line("DISTANCIA_IMPRUDENTE");
            elsif (dis_sin_act = DISTANCIA_INSEGURA) then
               Put_Line("DISTANCIA_INSEGURA");
            else Put_Line("PELIGRO_COLISION");
            end if;
         end if;

         if (sint.leer_cabeza_sin = true) then 
            Put_Line("SINTOMA CABEZA DETECTADO");
         end if;

         if (sint.leer_vol_sin = true) then
            Put_Line("SINTOMA VOLANTAZO DETECTADO");
         end if;

         Finishing_Notice("FIN TAREA DISPLAY");
         delay until sig_instante;
         sig_instante := sig_instante + intervalo;
      end loop;

   end Display;


   task body Giros is
      current_g: Steering_Samples_Type := 0;
      old_g: Steering_Samples_Type := 0;
      current_speed: Speed_Samples_Type := 0;
      sig_instante : Time;
      intervalo : Time_Span := To_Time_Span(0.35);
   begin
      sig_instante := Big_Bang + intervalo;
      loop
         Starting_Notice("COMIENZA TAREA GIROS");     

         Reading_Steering(current_g);
         Reading_Speed(current_speed);

         if (current_g >= old_g + 20) or (current_g <= old_g-20) then
            if (current_speed > 40) then
               sint.escribir_sint_vol(true);
            else 
               sint.escribir_sint_vol(false);
            end if;
         else
            sint.escribir_sint_vol(false);
         end if;

         old_g := current_g;
         Finishing_Notice("FIN TAREA GIROS");
         delay until sig_instante;
         sig_instante := sig_instante + intervalo;
      end loop;
   end Giros;

   task body Riesgos is
      sig_instante : Time;
      intervalo : Time_Span := To_Time_Span(0.15);
      modo : Integer;
      vol_sin: boolean;
      cab_sin: boolean;
      dis_sin: Integer;
      begin
      sig_instante := Big_Bang + intervalo;
      loop
         Starting_Notice("COMIENZA TAREA RIESGOS");     
         modo := interrupt_handler.leer_modo;
         vol_sin := sint.leer_vol_sin;
         cab_sin := sint.leer_cabeza_sin;
         dis_sin := sint.leer_dist_sin;
         if (modo /= M3) then
            if (vol_sin) then
               Beep(1);
            end if;

            if(cab_sin) then
               Beep(2);
               if(Integer(medida.leer_velocidad) >= 70)then
                  Beep(3); 
               end if;
            end if;
            
            if(dis_sin = DISTANCIA_INSEGURA) AND (modo /= M2)
               then Light(ON); 
            end if;

            if(dis_sin = DISTANCIA_IMPRUDENTE) AND (modo /= M2)
            then
               Beep(4);
               Light(ON);
            end if;
            
            if(dis_sin = PELIGRO_COLISION) AND (cab_sin) then
               Beep(5);
               Activate_Brake;
            end if;
         end if;
         
         Finishing_Notice("FIN TAREA RIESGOS");
         delay until sig_instante;
         sig_Instante := sig_instante + intervalo;
      end loop;
    end Riesgos;

begin
   Starting_Notice ("Programa Principal");
   Finishing_Notice ("Programa Principal");
end add;