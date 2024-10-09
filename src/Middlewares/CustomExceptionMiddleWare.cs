using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;


//Classe criada para personalizar mensagem aquando do erro 403
public class CustomExceptionMiddleware
{
    private readonly RequestDelegate _next;

    public CustomExceptionMiddleware(RequestDelegate next)
    {
        _next = next;
    }

    public async Task Invoke(HttpContext context)
    {
        await _next(context);

        if (context.Response.StatusCode == StatusCodes.Status403Forbidden)
        {
            context.Response.ContentType = "application/json";
            await context.Response.WriteAsync(new 
            {
                StatusCode = context.Response.StatusCode,
                Message = "Access denied. You do not have permission to access this resource."
            }.ToString());
        }
    }
}

