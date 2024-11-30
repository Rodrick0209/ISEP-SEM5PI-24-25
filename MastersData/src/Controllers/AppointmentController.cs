using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using Microsoft.AspNetCore.Authorization;
using System;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class AppointmentController : ControllerBase
    {
        private readonly IAppointmentService _service;

        private readonly IOperationRoomService _op_service;

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
        public async Task<ActionResult<AppointmentDto>> GetGetById(String id)
        {
            var op = await _service.GetByIdAsync(new AppointmentId(id));
            if (op == null)
            {
                return NotFound();
            }
            return op;
        }




    }
}


