using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Families;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Authorization;


namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]


    public class UsersController : ControllerBase
    {

        private readonly UserService _service;
        
        public UsersController(UserService service)
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
        [Authorize(Roles = "admin")]
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
                
                var token =  _service.GenerateResetPasswordToken(user);

                if (token == null)
                    return BadRequest("Something went wrong");
    
                
                
                string callbackUrl = $"http://localhost:9999/resetpassword?code={token}&Email={user.email.email}";

                await _service.sendEmail(user.email.email, callbackUrl);
                Console.WriteLine("Parte do email passada"); 

                return Ok(new
                {
                    token = token,
                    email = user.email.email,
                });



            }return BadRequest(ModelState);



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

            var result = await _service.ResetPassword(user, dto.NewPassword,dto.Token);
            
            if(result.Equals("Success"))
                return Ok("Password reset successful");


            return BadRequest("Something went wrong");    

        }



    }




}