using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Patients;
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
        public async Task<ActionResult<PatientDto>> Create(CreatingPatientProfileDto dto)
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

        // PUT: api/Patients/{medicalRecordNumber}
        /* [HttpPut("{medicalRecordNumber}")]
         public async Task<ActionResult<PatientDto>> Replace(string medicalRecordNumber, ReplacingPatientProfileDto dto)
         {
             if (medicalRecordNumber != dto.MedicalRecordNumber)
             {
             return BadRequest();
             }

             try
             {
             var patient = await _service.ReplaceAsync(dto);

             return Ok(patient);
             }
             catch (Exception ex)
             {
             return BadRequest(new { Message = ex.Message });
             }
         }
         */

        // PATCH: api/Patients/{medicalRecordNumber}
        [HttpPatch("{medicalRecordNumber}")]
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
        public async Task<ActionResult<IEnumerable<ViewPatientDto>>> SearchAsync(SearchFiltersDto dto)
        {
            return await _service.SearchAsync(dto);
        }

        // GET: api/Patients
        [HttpGet]
        public async Task<ActionResult<IEnumerable<PatientDto>>> GetAllAsync()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Patients/MedicalRecordNumber/{medicalRecordNumber}
        [HttpGet("MedicalRecordNumber/{medicalRecordNumber}")]
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