using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using Microsoft.AspNetCore.Authorization;
using System;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class AppointmentController : ControllerBase
    {
        private readonly IAppointmentService _service;

        private readonly IOperationRoomService _op_service;
        private readonly IOperationRequestService or_service;

        public AppointmentController(AppointmentService service, OperationRoomService op_Service)
        {
            _service = service;
            _op_service = op_Service;
        }

        // GET: api/OperationRoom/GetAll
        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<AppointmentDto>>> GetAll()
        {
            var list = await _service.GetAllAsync();
            var listDto = new List<AppointmentDto>();

            var rooms = await _op_service.GetAllAsync();

            var roomMap = new Dictionary<OperationRoomId, RoomNumber>();
            foreach (var room in rooms)
            {
                roomMap[room.Id] = room.RoomNumber;
            }


            foreach (var app in list)
            {
                listDto.Add(AppointmentMapper.ToDTO(app, roomMap));
            }

            return Ok(listDto);
        }

        [HttpGet("GetHappening")]
        public async Task<ActionResult<IEnumerable<AppointmentDto>>> GetHappening()
        {
            var list = await _service.GetAllAsync();

            var rooms = await _op_service.GetAllAsync();

            var roomMap = new Dictionary<OperationRoomId, RoomNumber>();
            foreach (var room in rooms)
            {
                roomMap[room.Id] = room.RoomNumber;
            }

            var listDto = new List<AppointmentDto>();

            foreach (var app in list)
            {
                if (app.isHappening())
                {
                    listDto.Add(AppointmentMapper.ToDTO(app, roomMap));
                }

            }

            return Ok(listDto);
        }



        [HttpPost]
        public async Task<ActionResult<AppointmentDto>> Create(CreatingAppointmentDto dto)
        {

            try
            {
                var app = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = app.AppointmentId }, app);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }

        }


        [HttpPost("CreateWithMedicalTeam")]
        public async Task<ActionResult<AppointmentDto>> CreateWithMedicalTeam(CreateAppointmentWithMedicalTeam dto)
        {

            try
            {
                Console.WriteLine("ID DA SALA" + dto.OperationRoomId);
                var app = await _service.AddWithMedicalTeamAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = app.AppointmentId }, app);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }

        }


        [HttpGet("GetStaffAvailableForDoinSurgeryAtCertainTime")]
        public async Task<ActionResult<StaffForSurgeryDto>> GetStaffAvailableForDoinSurgeryAtCertainTime(
            [FromQuery] GetMedicalSurgeryParamsDto dto)
        {
            try
            {
                Console.WriteLine("Entrou no controller method");
                Console.WriteLine(dto.startMinute);
                Console.WriteLine(dto.date);
                Console.WriteLine(dto.operationRequestId);

                return await _service.GetStaffAvailableForDoinSurgeryAtCertainTime(dto.startMinute, dto.date, dto.operationRequestId);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message, ex.StackTrace });
            }
        }






        [HttpPut("{id}")]
        [Authorize(Roles = "doctor")]
        public async Task<ActionResult<AppointmentDto>> Update(EditingAppointmentDto dto, Guid id)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var app = await _service.UpdateAsync(dto);

                return Ok(app);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<AppointmentDtoUI>> GetGetById(String id)
        {
            var op = await _service.GetByIdAsync(new AppointmentId(id));
            if (op == null)
            {
                return NotFound();
            }
            return op;
        }

        [HttpGet("GetAllUI")]
        public async Task<ActionResult<IEnumerable<AppointmentDtoUI>>> GetAllUI()
        {
            return await _service.GetAllForUIAsync();
        }

        [HttpGet("medicalRecordNumber/{medicalRecordNumber}")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult<IEnumerable<AppointmentDtoInTable>>> GetByMedicalRecordNumberAsync(string medicalRecordNumber)
        {
            return await _service.GetByMedicalRecordNumberAsync(medicalRecordNumber);
        }



      
    }



}


