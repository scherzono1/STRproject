
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
    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    task Cabeza is 
    pragma priority(4);
    end Cabeza;

    task Distancia is
    pragma priority(2);
    end Distancia;

    task Giros is
    pragma priority(3);
    end Giros;

    task Riesgos is 
    pragma priority(3);
    end Riesgos;

    ---------------------------------------------------------
    -- Aqui se declaran las tareas que forman el STR
    ---------------------------------------------------------
    ----------------------------------------------------------------------
    ------------- declaracion del tipo de objeto protegido 
    ----------------------------------------------------------------------
    
    protected type Sintomas(valor_ini: integer) is 
    	pragma priority(4);
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
    	pragma priority(13);
    	procedure escribir_dist_vel(dist: Distance_Samples_Type; vel: Speed_Samples_Type);
      function leer_distancia return Distance_Samples_Type;
      function leer_velocidad return Speed_Samples_Type;
    private
    	velocidad: Speed_Samples_Type;
    	distancia: Distance_Samples_Type;
    end Medidas;

    sint: Sintomas(0);
    medida: Medidas;
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
    
    t_start : Time;
    t_end : Time;
    
    begin
    t_sig := Big_Bang + intervalo;
    loop
    Starting_Notice("COMENZANDO TAREA CABEZA");
    
    t_start := Clock;
    
      Reading_HeadPosition (cabeza_act);
      Display_HeadPosition_Sample (cabeza_act);
      Reading_Steering (sw_act);
      Display_Steering (sw_act);

      IF(((cabeza_act(x) > 30) and (cabeza_ant(x) > 30)) OR
        ((cabeza_act(x) < -30) and (cabeza_ant(x) < -30)) OR
        (((cabeza_act(y) > 30) and (cabeza_ant(y) > 30)) AND ((sw_act < 5 and sw_ant < 5))) OR
        (((cabeza_act(y) < -30) and (cabeza_ant(y) < -30)) AND ((sw_act > -5 and sw_ant > -5))))

      THEN sint.escribir_cabeza_sint(true);

      ELSE sint.escribir_cabeza_sint(false);

      END IF;

      cabeza_ant := cabeza_act;
      sw_ant := sw_act;
      
      t_end := Clock;
      Put_Line("Tiempo total de la tarea Cabeza: "& Duration'Image(To_Duration(t_end - t_start)));
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

   task body Giros is
    current_g: Steering_Samples_Type := 0;
    old_g: Steering_Samples_Type := 0;
    current_speed: Speed_Samples_Type := 0;
    sig_instante : Time;
    intervalo : Time_Span := To_Time_Span(0.35);
   begin
   sig_instante := Big_Bang + intervalo;
      loop
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
         delay until sig_instante;
         sig_instante := sig_instante + intervalo;
      end loop;
   end Giros;


   
   task body Riesgos is
    sig_instante : Time;
    intervalo : Time_Span := To_Time_Span(0.15);
    begin
    sig_instante := Big_Bang + intervalo;
      loop
         Starting_Notice("COMIENZA TAREA RIESGOS");         
         if (sint.leer_vol_sin) then
            Beep(1);
         end if;

         if(sint.leer_cabeza_sin) then
            Beep(2);
            if(Integer(medida.leer_velocidad) >= 70)then
               Beep(3); 
            end if;
         end if;
         
         if(sint.leer_dist_sin = DISTANCIA_INSEGURA)
            then Light(ON); 
         end if;

         if(sint.leer_dist_sin = DISTANCIA_IMPRUDENTE) then
            Beep(4);
            Light(ON);
         end if;
         
         if(sint.leer_dist_sin = PELIGRO_COLISION) AND (sint.leer_cabeza_sin) then
            Beep(5);
            Activate_Brake;
         end if;
         
         Finishing_Notice("FIN TAREA RIESGOS");
         delay until sig_instante;
         sig_Instante := sig_instante + intervalo;
      end loop;
    end Riesgos;

    ----------------------------------------------------------------------
    ------------- procedure para probar los dispositivos 
    ----------------------------------------------------------------------
    procedure Prueba_Dispositivos; 

    Procedure Prueba_Dispositivos is
        Current_V: Speed_Samples_Type := 0;
        Current_H: HeadPosition_Samples_Type := (+2,-2);
        Current_D: Distance_Samples_Type := 0;
        Current_O: Eyes_Samples_Type := (70,70);
        Current_E: EEG_Samples_Type := (1,1,1,1,1,1,1,1,1,1);
        Current_S: Steering_Samples_Type := 0;
    begin
         Starting_Notice ("Prueba_Dispositivo");

         for I in 1..120 loop
         -- Prueba distancia
            --Reading_Distance (Current_D);
            --Display_Distance (Current_D);
            --if (Current_D < 40) then Light (On); 
            --                    else Light (Off); end if;

         -- Prueba velocidad
            --Reading_Speed (Current_V);
            --Display_Speed (Current_V);
            --if (Current_V > 110) then Beep (2); end if;

         -- Prueba volante
         --   Reading_Steering (Current_S);
         --   Display_Steering (Current_S);
         --   if (Current_S > 30) OR (Current_S < -30) then Light (On);
         --                                            else Light (Off); end if;

         -- Prueba Posicion de la cabeza
            --Reading_HeadPosition (Current_H);
            --Display_HeadPosition_Sample (Current_H);
            --if (Current_H(x) > 30) then Beep (4); end if;

         -- Prueba ojos
            --Reading_EyesImage (Current_O);
            --Display_Eyes_Sample (Current_O);

         -- Prueba electroencefalograma
            --Reading_Sensors (Current_E);
            --Display_Electrodes_Sample (Current_E);
   
         delay until (Clock + To_time_Span(0.1));
         end loop;

         Finishing_Notice ("Prueba_Dispositivo");
    end Prueba_Dispositivos;


begin
   Starting_Notice ("Programa Principal");
   Prueba_Dispositivos;
   Finishing_Notice ("Programa Principal");
end add;



