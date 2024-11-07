using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class AppointmentController : ControllerBase
    {
        private readonly IAppointmentService _service;

        public AppointmentController(AppointmentService service)
        {
            _service = service;
        }

        // GET: api/OperationRoom/GetAll
        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<AppointmentDto>>> GetAll()
        {
            var list = await _service.GetAllAsync();
            var listDto = new List<AppointmentDto>();

            foreach (var app in list)
            {
                listDto.Add(AppointmentMapper.ToDTO(app));
            }

            return Ok(listDto);
        }
    }
}


