using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationType;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    
    public class OperationTypeController : ControllerBase
    {
        private readonly IOperationTypeService _service;

        public OperationTypeController(OperationTypeService service)
        {
            _service = service;
        }

        // POST: api/OperationType
        [HttpPost("Create")]
        public async Task<ActionResult<OperationTypeDto>> Create(OperationTypeDto dto)
        {
            var objDomain = OperationTypeMapper.toDomain(dto);
            var op = await _service.CreateAsync(objDomain);
            var op2 = OperationTypeMapper.ToDto(op);
            return CreatedAtAction(nameof(GetGetById), new { id = op2.Id }, op2);

        }

        [HttpGet("{id}")]
        public async Task<ActionResult<OperationTypeDto>> GetGetById(String id)
        {
            var op = await _service.GetByIdAsync(new OperationTypeId(id));
            if (op == null)
            {
                return NotFound();
            }
            return OperationTypeMapper.ToDto(op);
        }
    }
}