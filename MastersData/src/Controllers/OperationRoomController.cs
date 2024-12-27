using System.Threading.Tasks;
using DDDSample1.Domain.OperationRooms;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using DDDSample1.Domain.Appointments;
using System;
using DDDSample1.Domain.Patients;
using Org.BouncyCastle.Asn1.Icao;
using System.Linq;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class OperationRoomController : ControllerBase
    {
        private readonly IOperationRoomService _service;

        private readonly IAppointmentService _app_service;
        private readonly IOperationRequestService _or_service;
        private readonly IPatientService _pt_service;
        private readonly IOperationTypeService _ot_service;


        public OperationRoomController(OperationRoomService service, AppointmentService appointmentService, OperationRequestService orService, IPatientService pt, IOperationTypeService op_Ser)
        {
            _service = service;
            _app_service= appointmentService;
            _or_service= orService;
            _pt_service=pt;
            _ot_service=op_Ser;
        }


        // GET: api/OperationRoom/GetAll
        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<OperationRoomDto>>> GetAll()
        {
            var list = await _service.GetAllAsync();
            var listDto = new List<OperationRoomDto>();

            foreach (var room in list)
            {
                listDto.Add(OperationRoomMapper.ToDTO(room));
            }

            return Ok(listDto);
        }

        // GET: api/OperationRoom/OccupiedRooms
        [HttpGet("OccupiedRooms")]
        public async Task<ActionResult<IEnumerable<OperationRoomDto>>> GetOccupiedRooms([FromQuery] DateOnly date, [FromQuery] TimeOnly time)
        {
            if (date == default)
            {
                date = DateOnly.FromDateTime(DateTime.Now);
            }

            if (time == default)
            {
                time = TimeOnly.FromDateTime(DateTime.Now);
            }
            var occupiedRooms = await _service.GetOccupiedAsync(date, time);
            var occupiedRoomsDto = new List<OperationRoomDto>();

            foreach (var room in occupiedRooms)
            {
                occupiedRoomsDto.Add(OperationRoomMapper.ToDTO(room));
            }

            return Ok(occupiedRoomsDto);
        }

        [HttpGet("GetPatientDataForAppointment")]
        public async Task<ActionResult<PatientAndTypeDto>> GetPatientDataForAppointment(
    [FromQuery] DateOnly date,
    [FromQuery] TimeOnly time,
    [FromQuery] string roomName)
        {
            // Obter salas ocupadas no horário especificado
            var result = await _service.GetOccupiedAsync(date, time);

            // Verificar se o resultado e os dados retornados são válidos
            if (result == null)
            {
                return NotFound("No data found for occupied rooms.");
            }

            AppointmentId? appointmentId = null;
            int timeInMinutes = time.Hour * 60 + time.Minute;

            // Buscar a sala pelo nome e identificar o agendamento correspondente
            foreach (var room in result)
            {

                if (room.RoomNumber.roomNumber == roomName)
                {
                    var appointment = room.Appointments.FirstOrDefault(a =>
                        a.AppointmentTimeSlot.Date == date &&
                        timeInMinutes >= a.AppointmentTimeSlot.TimeSlot.StartMinute &&
                        timeInMinutes <= a.AppointmentTimeSlot.TimeSlot.EndMinute);

                    if (appointment != null)
                    {
                        appointmentId = appointment.Id;
                        break;
                    }
                }
            }

            Appointment patientData = null;
            Console.WriteLine(appointmentId.Value);
            Console.WriteLine(new AppointmentId(appointmentId.Value).Value);

            // Caso nenhum agendamento seja encontrado
            if (appointmentId == null)
            {
                return NotFound("No appointment found for the given date, time, and room.");
            }
            else
            {
                patientData = await _app_service.GetByIdStringAsync(appointmentId.Value);
            }
            // Obter os dados do paciente associado ao ID do agendamento

            if (patientData == null)
            {
                return NotFound("No patient data found for the appointment.");
            }

            var request = await _or_service.GetByIdAsync(patientData.OperationRequestId);
            var patient = _pt_service.GetByIdAsync(new PatientId(request.patientId));
            
            var type= await _ot_service.GetByIdAsync(new OperationTypeId(request.operationTypeId));
            
            var patientDto = new PatientAndTypeDto(
                patient.Result,
                type.name
            );
            
            return Ok(patientDto);
        }
    }
}