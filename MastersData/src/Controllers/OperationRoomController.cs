using System.Threading.Tasks;
using DDDSample1.Domain.OperationRooms;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class OperationRoomController : ControllerBase
    {
        private readonly IOperationRoomService _service;

        public OperationRoomController(OperationRoomService service)
        {
            _service = service;
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
    }
}


