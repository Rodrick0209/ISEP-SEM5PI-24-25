
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Application.Dtos;
using DDDSample1.Application.Mappers;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{

    [Route("api/[controller]")]
    [ApiController]

    public class SpecializationsController : ControllerBase
    {

        private readonly ISpecializationService _service;

        public SpecializationsController(ISpecializationService service)
        {
            _service = service;
        }


        [HttpPost]

        public async Task<ActionResult<SpecializationDto>> Create(SpecializationDto dto)
        {
            try
            {
                var op = await _service.CreateAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = op.Id }, op);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<SpecializationDto>> GetGetById(Guid id)
        {
            var specialization = await _service.GetByIdAsync(new SpecializationId(id));

            if (specialization == null)
            {
                return NotFound();
            }

            return SpecializationMapper.ToDto(specialization);
        }


        [HttpGet("GetAll")]

        public async Task<ActionResult<IEnumerable<SpecializationDto>>> GetAll()
        {
            var specializations = await _service.GetAllAsync();
            if (specializations == null)
            {
                return NotFound();
            }


            return Ok(specializations);
        }


        [HttpGet("GetFiltered")]
        public async Task<ActionResult<IEnumerable<SpecializationDto>>> GetFiltered([FromQuery] SpecializationFilterDto dto)
        {
            Console.WriteLine(dto.Name);
            var specializations = await _service.GetFilteredAsync(dto);
            if (specializations == null || specializations.Count == 0)
            {
                return NotFound();
            }

            return Ok(specializations);
        }





    }








}