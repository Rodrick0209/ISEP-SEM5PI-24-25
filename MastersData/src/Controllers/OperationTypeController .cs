using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;

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
            return CreatedAtAction(nameof(GetById), new { id = op2.Id }, op2);

        }

        [HttpGet("{id}")]
        public async Task<ActionResult<OperationTypeDto>> GetById(String id)
        {
            var op = await _service.GetByIdAsync(new OperationTypeId(id));
            if (op == null)
            {
                return NotFound();
            }
            return OperationTypeMapper.ToDto(op);
        }

        [HttpDelete("{id}")]
        public async Task<ActionResult<OperationTypeDto>> Inactivate(string id)
        {

            try
            {
                var op = await _service.Deactivate(new OperationTypeId(id));
                if (op == null)
                {
                    return NotFound();
                }
                return Ok(OperationTypeMapper.ToDto(op));

            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<OperationTypeDto>>> GetAll()
        {
            var list = await _service.GetAllAsync();
            return Ok(list);
        }
        
    }
}