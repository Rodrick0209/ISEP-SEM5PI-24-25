using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;

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
            _op_service=op_Service;
        }

        // GET: api/OperationRoom/GetAll
        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<AppointmentDto>>> GetAll()
        {
            var list = await _service.GetAllAsync();
            var listDto = new List<AppointmentDto>();

            var rooms = await _op_service.GetAllAsync();

            var roomMap = new Dictionary<string, RoomNumber>();
            foreach (var room in rooms)
            {
                roomMap[room.Id.Value] = room.RoomNumber;
            }


            foreach (var app in list)
            {
                listDto.Add(AppointmentMapper.ToDTO(app,roomMap));
            }

            return Ok(listDto);
        }

        [HttpGet("GetHappening")]
        public async Task<ActionResult<IEnumerable<AppointmentDto>>> GetHappening()
        {
            var list = await _service.GetAllAsync();

            var rooms = await _op_service.GetAllAsync();

            var roomMap = new Dictionary<string, RoomNumber>();
            foreach (var room in rooms)
            {
                roomMap[room.Id.Value] = room.RoomNumber;
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
    }
}


