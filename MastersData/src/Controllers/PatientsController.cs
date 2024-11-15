using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]

    public class PatientsController : ControllerBase
    {
        private readonly IPatientService _service;

        public PatientsController(IPatientService service)
        {
            _service = service;
        }

        // POST: api/Patients
        [HttpPost]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<PatientDto>> CreateAsync(CreatingPatientProfileDto dto)
        {
            try
            {
                var patient = await _service.CreateAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = patient.Id }, patient);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PATCH: api/Patients/{medicalRecordNumber}
        [HttpPatch("{medicalRecordNumber}")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<PatientDto>> UpdateAsync(string medicalRecordNumber, EditingPatientProfileDto dto)
        {
            if (medicalRecordNumber != dto.MedicalRecordNumber)
            {
                return BadRequest();
            }

            try
            {
                var patient = await _service.UpdateAsync(dto);

                return Ok(patient);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/Patients/{medicalRecordNumber}
        [HttpDelete("{medicalRecordNumber}")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult> DeleteAsync(string medicalRecordNumber)
        {
            try
            {
                await _service.DeleteAsync(medicalRecordNumber);

                return NoContent();
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        //GET: api/Patients/search
        [HttpGet("search")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<IEnumerable<ViewPatientDto>>> SearchAsync([FromQuery] SearchFiltersDto dto)
        {
            return await _service.SearchAsync(dto);
        }

        // GET: api/Patients
        [HttpGet]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<IEnumerable<PatientDto>>> GetAllAsync()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Patients/MedicalRecordNumber/{medicalRecordNumber}
        [HttpGet("MedicalRecordNumber/{medicalRecordNumber}")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<PatientDto>> GetByMedicalRecordNumberAsync(string medicalRecordNumber)
        {
            var patient = await _service.GetByMedicalRecordNumberAsync(medicalRecordNumber);

            if (patient == null)
            {
                return NotFound();
            }

            return patient;
        }

        // GET: api/Patients/{id}
        [HttpGet("{id}")]
        [Authorize(Roles = "admin")]
        public async Task<ActionResult<PatientDto>> GetGetById(Guid id)
        {
            var patient = await _service.GetByIdAsync(new PatientId(id));

            if (patient == null)
            {
                return NotFound();
            }

            return patient;
        }

    }
}