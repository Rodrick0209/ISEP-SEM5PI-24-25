using System;
using System.Threading.Tasks;
using DDDSample1.Domain.RoomTypes;
using MastersData.Domain.RoomTypes;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]

    public class RoomTypesController : ControllerBase
    {
        private readonly IRoomTypeService _service;

        public RoomTypesController(IRoomTypeService service)
        {
            _service = service;
        }

        // POST: api/RoomTypes
        [HttpPost]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<RoomTypeDto>> AddAsync(AddRoomTypeDto dto)
        {
            try
            {
                var roomType = await _service.AddRoomTypeAsync(dto);

                return CreatedAtAction(nameof(GetById), new { id = roomType.Id.ToString() }, roomType);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // GET: api/RoomTypes/{id}
        [HttpGet("{id}")]
        public async Task<ActionResult<RoomTypeDto>> GetById(Guid id)
        {
            var roomType = await _service.GetByIdAsync(new RoomTypeId(id));

            if (roomType == null)
            {
                return NotFound();
            }

            return Ok(roomType);
        }
    
        // GET: api/RoomTypes
        [HttpGet]
        public async Task<ActionResult<RoomTypeDto>> GetAll()
        {
            var list = await _service.GetAllAsync();

            return Ok(list);
        }

    }
}