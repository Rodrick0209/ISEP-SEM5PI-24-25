using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System.Linq;
using Microsoft.AspNetCore.Routing.Constraints;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]

    public class OperationTypeController : ControllerBase
    {
        private readonly IOperationTypeService _service;
        private readonly ISpecializationService _Spe_service;

        public OperationTypeController(IOperationTypeService service, ISpecializationService spe_service)
        {
            _service = service;
            _Spe_service = spe_service;
        }

        // POST: api/OperationType
        [HttpPost("Create")]
        public async Task<ActionResult<OperationTypeDto>> Create(OperationTypeDto dto)
        {
            if (dto == null)
            {
                return BadRequest("OperationTypeDto cannot be null");
            }
            var objDomain = OperationTypeMapper.toDomain(dto);
            
            var op = await _service.CreateAsync(objDomain);

            
            return await GetById(op.Id.Value);
           
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<OperationTypeDto>> GetById(string id)
        {
            var op = await _service.GetByIdAsync(new OperationTypeId(id));

            if (op == null)
            {
                return NotFound();
            }

            
            /*
            var sp = new SpecializationId(op.specialization.Value);
            var specialization = await _Spe_service.GetByIdAsync(sp);
            var specializationNames = await _Spe_service.GetByNameOperationTypeAsync(op);
            var opDto = OperationTypeMapper.ToDto(op, specialization.Name, specializationNames);
            */
            var opDto = OperationTypeMapper.ToDto(op);
            return Ok(opDto);

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
                return await GetById(op.Id.Value);
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
            var listDto = new List<OperationTypeDto>();

            foreach (var op in list)
            {
                var result = await GetById(op.Id.Value);
                if (result.Result is OkObjectResult okResult && okResult.Value is OperationTypeDto dto)
                {
                    listDto.Add(dto);
                }
            }

            return Ok(listDto);
        }


        [HttpPut("{id}")]
        public async Task<ActionResult<OperationTypeDto>> Update(Guid id, OperationTypeDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var op = await _service.UpdateAsync(dto);
                if (op == null)
                {
                    return NotFound();
                }
                return await GetById(op.Id.Value);

            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        [HttpGet("Filter")]
        public async Task<ActionResult<IEnumerable<OperationTypeDto>>> GetByFilters(string name, string status, string specialization)
        {
            var specializationstr="";
            if (!string.IsNullOrEmpty(specialization))
            {
                var specializationId = await _Spe_service.GetByNameAsync(specialization);
                specializationstr=specializationId.Id.Value;
            }
            

            var list = await _service.GetOperationTypesByFilter(name,status,specializationstr);

            var listDto = new List<OperationTypeDto>();

            foreach (var op in list)
            {
                var result = await GetById(op.Id.Value);
                if (result.Result is OkObjectResult okResult && okResult.Value is OperationTypeDto dto)
                {
                    listDto.Add(dto);
                }
            }

            return Ok(listDto);
        }

    }


    
}