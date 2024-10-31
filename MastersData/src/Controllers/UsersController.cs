using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authorization;
using DDDSample1.Domain.Patients;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]


    public class UsersController : ControllerBase
    {

        private readonly IUserService _service;

        public UsersController(IUserService service)
        {
            _service = service;
        }




        // POST: api/user
        [HttpPost("Create user")]
        public async Task<ActionResult<UserDTO>> Create(UserDTO dto)
        {
            try
            {

                if (!ModelState.IsValid) return BadRequest(ModelState);

                var user = await _service.AddAsync(UserMapper.ToDomain(dto));

                return UserMapper.ToDto(user);

            }
            catch (BusinessRuleValidationException ex)
            {
                return Conflict(new { Message = ex.Message });
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // GET: api/user
        [HttpGet("Get user")]
        public async Task<List<UserDTO>> GetAllAsync()
        {

            var list = await _service.GetAllAsync();
            List<UserDTO> listDto = list.ConvertAll<UserDTO>(User =>
                UserMapper.ToDto(User));

            return listDto;
        }


        [Route("Forgot Password")]
        [HttpPost]
        public async Task<ActionResult> ForgotPassword(RequestForgotPasswordDto dto)
        {


            if (ModelState.IsValid)
            {
                var user = await _service.GetByEmailAsync(dto.Email);

                if (user == null)
                {
                    return BadRequest("Invalid payload");
                }

                var token = _service.GenerateResetPasswordToken(user);

                if (token == null)
                    return BadRequest("Something went wrong");



                string callbackUrl = $"http://localhost:9999/resetpassword?code={token}&Email={user.email.email}";

                await _service.sendEmailWithUrlResetPassword(user.email.email, callbackUrl);
                Console.WriteLine("Parte do email passada");

                return Ok(new
                {
                    token = token,
                    email = user.email.email,
                });



            }
            return BadRequest(ModelState);



        }

        [Route("Reset Password")]
        [HttpPost]
        public async Task<ActionResult> ResetPassword(ResetPasswordRequestDto dto)
        {
            if (!ModelState.IsValid)
                return BadRequest("Invalid payload");

            var user = await _service.GetByEmailAsync(dto.Email);
            if (user == null)
            {
                return BadRequest("Invalid payload");
            }

            var result = await _service.ResetPassword(user, dto.NewPassword, dto.Token);

            if (result.Equals("Success"))
                return Ok("Password reset successful");


            return BadRequest("Something went wrong");

        }

        // POST: api/user/patients
        [HttpPost("patients")]
        public async Task<ActionResult<ConfirmationPatientDto>> RegisterPatientAsync(RegisteringPatientDto dto)
        {
            try
            {
                var confirmationRegisterPatientDto = await _service.RegisterPatientAsync(dto);

                return Ok(confirmationRegisterPatientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // POST: api/user/patients/confirmation/{token}
        [HttpPost("patients/confirmation/{token}")]
        public async Task<ActionResult<PatientDto>> ConfirmRegisterPatientAsync(string token, ConfirmationPatientDto dto)
        {

            if (token != dto.Token)
            {
                return NotFound();
            }

            try
            {
                var userDto = await _service.ConfirmRegisterPatientAsync(dto);

                return Ok(userDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // PATCH : api/user/patients/edit
        [HttpPatch("patients/edit")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult<ConfirmationEditPatientDto>> EditPatientAsync(EditingPatientDto dto)
        {

            try
            {
                var confirmationEditPatientDto = await _service.EditPatientAsync(dto);

                return Ok(confirmationEditPatientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // PATCH : api/user/patients/edit/confirmation/{token}
        [HttpPatch("patients/edit/confirmation/{token}")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult<PatientDto>> ConfirmEditPatientAsync(string token, ConfirmationEditPatientDto dto)
        {

            if (token != dto.Token)
            {
                return NotFound();
            }

            try
            {
                var patientDto = await _service.ConfirmEditPatientAsync(dto);

                return Ok(patientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // DELETE: api/user/patients/delete
        [HttpDelete("patients/delete")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult<DeletingPatientProfileConfirmationDto>> DeletePatientAsync(DeletingPatientDto dto)
        {
            try
            {
                var confirmationDeletePatientDto = await _service.DeletePatientAsync(dto);

                return Ok(confirmationDeletePatientDto);
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // DELETE: api/user/patients/delete/confirmation/{token}
        [HttpDelete("patients/delete/confirmation/{token}")]
        [Authorize(Roles = "patient")]
        public async Task<ActionResult> ConfirmDeletePatientAsync(string token, ConfirmationPatientDto dto)
        {

            if (token != dto.Token)
            {
                return NotFound();
            }

            try
            {
                await _service.ConfirmDeletePatientAsync(dto);

                return NoContent();
            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.ToString() });
            }
        }

        // GET: api/user/{id}
        [HttpGet("{id}")]
        public async Task<ActionResult<UserDTO>> GetByIdAsync(Guid id)
        {
            var user = await _service.GetByIdAsync(new UserId(id));

            if (user == null)
            {
                return NotFound();
            }

            return Ok(user);
        }
    }
}
