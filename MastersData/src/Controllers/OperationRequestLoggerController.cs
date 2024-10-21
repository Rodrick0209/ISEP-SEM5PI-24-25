
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRequestLoggers;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{

    [Route("api/[controller]")]
    [ApiController]


    public class OperationRequestLoggerController : ControllerBase
    {

        private readonly OperationRequestLoggerService _service;


        public OperationRequestLoggerController(OperationRequestLoggerService service)
        {
            _service = service;
        }

        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<OperationRequestLoggerDto>>> GetAll()
        {
            var list = await  _service.GetAllAsync();
            var listDto = new List<OperationRequestLoggerDto>();
            
            foreach (var op in list)
            {
                listDto.Add(OperationRequestLoggerMapper.toDTO(op));
            }

            return Ok(listDto);
        } 




    }




}