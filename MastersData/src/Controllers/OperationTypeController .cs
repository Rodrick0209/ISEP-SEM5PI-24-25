using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]

    public class OperationTypeController : ControllerBase
    {
        private readonly IOperationTypeService _service;
        private readonly SpecializationService _Spe_service;

        public OperationTypeController(OperationTypeService service, SpecializationService spe_service)
        {
            _service = service;
            _Spe_service = spe_service;
        }

        // POST: api/OperationType
        [HttpPost("Create")]
        public async Task<ActionResult<OperationTypeDto>> Create(OperationTypeDto dto)
        {
            var objDomain = OperationTypeMapper.toDomain(dto);
            var op = await _service.CreateAsync(objDomain);
            var specialization = await _Spe_service.GetByIdAsync(new SpecializationId(dto.Specialization));
            var op2 = OperationTypeMapper.ToDto(op, specialization.Name);
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

            
            var sp = new SpecializationId(op.specialization.Value);
            var specialization = await _Spe_service.GetByIdAsync(sp);
            
            if (specialization == null || string.IsNullOrEmpty(specialization.Name))
            {
                return BadRequest("Specialization not found or invalid");
            }

            return OperationTypeMapper.ToDto(op, specialization.Name);
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
                var specialization = await _Spe_service.GetByIdAsync(new SpecializationId(op.specialization.Value));
                return Ok(OperationTypeMapper.ToDto(op, specialization.Name));

            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

<<<<<<< HEAD
=======

        [HttpGet("GetAll")]
        public async Task<ActionResult<IEnumerable<OperationTypeDto>>> GetAll()
        {
            var list = await _service.GetAllAsync();
            return Ok(list);
        }
        
>>>>>>> 734860ea6fe291b5b962c4688c6111ec3d1016e4
    }
}