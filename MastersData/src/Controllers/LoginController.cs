using System.Threading;
using System.Threading.Tasks;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Identity.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using System;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using System.Security.Claims;
using Microsoft.AspNetCore.Authentication.Google;
using System.Linq;



namespace DDDSample1.Controllers
{

    [Route("api/[controller]")]
    [ApiController]
    public class LoginController : ControllerBase
    {
        private readonly IUserService _userService;


        public LoginController(IUserService service)
        {
            _userService = service;
        }

        [HttpGet("signin-google")]
        public IActionResult SignInGoogle()
        {
            var redirectUrl = Url.Action(nameof(GoogleResponse), "Login"); 
            var properties = new AuthenticationProperties
            {
                RedirectUri = redirectUrl,
                Items = { { "prompt", "select_account" } }  //isto obriga o Google a mostrar a tela de login, mesmo que o user já esteja logado
            }; 
            return Challenge(properties, GoogleDefaults.AuthenticationScheme);
        }

        [HttpGet("google-response")]
        public async Task<IActionResult> GoogleResponse()
        {
            // Obtém o usuário autenticado
            var authenticateResult = await HttpContext.AuthenticateAsync(CookieAuthenticationDefaults.AuthenticationScheme);

            if (authenticateResult.Succeeded)
            {
                var email = authenticateResult.Principal.FindFirstValue(ClaimTypes.Email);

                /* esta parte armazena o access token no cookie de autenticação, so precisamos disto se quisermos usar o token para fazer chamadas a API do Google
                var accessToken = authenticateResult.Properties.GetTokenValue("access_token");
                authenticateResult.Properties.StoreTokens(new[] {
                            new AuthenticationToken { Name = "access_token", Value = accessToken }
                        });
                */

                
                // Verifique se o email não é nulo
                if (string.IsNullOrEmpty(email))
                {
                    return BadRequest("Email not found in claims.");
                }

                var jwtToken = _userService.GenerateGoogleTokenFromJwt(email,"admin");

                return Ok(new { Token = jwtToken });
            }

            return BadRequest("Google authentication failed.");
        }



        [HttpPost("login")]
        public async Task<IActionResult> Login([FromBody] LoginRequest request)
        {
            try
            {
                var tokenString = await _userService.GetLogToken(request);
                return Ok(new { token = tokenString });
            }
            catch (Exception ex)
            {
                return Unauthorized(new { Message = ex.Message });
            }
        }




    }


}