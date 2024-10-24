
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Domain.StaffLoggers;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{

    [Route("api/[controller]")]
    [ApiController]


    public class StaffLoggerController : ControllerBase
    {

        private readonly StaffLoggerService _service;


        public StaffLoggerController(StaffLoggerService service)
        {
            _service = service;
        }

        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<StaffLoggerDto>>> GetAll()
        {
            var list = await  _service.GetAllAsync();
            var listDto = new List<StaffLoggerDto>();
            
            foreach (var op in list)
            {
                listDto.Add(StaffLoggerMapper.toDTO(op));
            }

            return Ok(listDto);
        } 




    }




}