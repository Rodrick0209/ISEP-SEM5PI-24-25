import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { AuthService } from './auth.service';


describe('AuthService', () => {
  let service: AuthService;
  let httpMock: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [AuthService]
    });
    service = TestBed.inject(AuthService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should login and return a token', () => {
    const mockResponse = { token: '12345' };
    service.login('test@example.com', 'password').subscribe(response => {
      expect(response.token).toEqual('12345');
    });

    const req = httpMock.expectOne('/api/Login/login');
    expect(req.request.method).toBe('POST');
    req.flush(mockResponse);
  });

  it('should verify Google token and return a result', () => {
    const mockResponse = { token: { result: 'success' } };
    service.verifyGoogleToken().subscribe(response => {
      expect(response.token.result).toEqual('success');
    });

    const req = httpMock.expectOne('/api/Login/google-response');
    expect(req.request.method).toBe('GET');
    req.flush(mockResponse);
  });

  it('should save token and update loggedIn status', () => {
    spyOn(localStorage, 'setItem');
    service.saveToken('12345');
    expect(localStorage.setItem).toHaveBeenCalledWith('token', '12345');
    service.isLoggedIn$.subscribe(isLoggedIn => {
      expect(isLoggedIn).toBeTrue();
    });
  });

  it('should get token from localStorage', () => {
    spyOn(localStorage, 'getItem').and.returnValue('12345');
    expect(service.getToken()).toEqual('12345');
  });


  it('should return loggedIn status based on token presence', () => {
    let getTokenSpy = spyOn(service, 'getToken').and.returnValue('12345');
    expect(service.isLoggedIn()).toBeTrue();
    getTokenSpy.and.returnValue(null);
    expect(service.isLoggedIn()).toBeFalse();
  });

  it('should clear token and update loggedIn status', () => {
    spyOn(localStorage, 'removeItem');
    service.clearToken();
    expect(localStorage.removeItem).toHaveBeenCalledWith('token');
    service.isLoggedIn$.subscribe(isLoggedIn => {
      expect(isLoggedIn).toBeFalse();
    });
  });

  it('should extract email from token', () => {
    const mockToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InRlc3RAZXhhbXBsZS5jb20ifQ.s5cX5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q';
    spyOn(service, 'getToken').and.returnValue(mockToken);
    expect(service.extractEmailFromToken()).toEqual('test@example.com');
  });

  it('should extract role from token', () => {
    const mockToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoiYWRtaW4ifQ.s5cX5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q5Q';
    spyOn(service, 'getToken').and.returnValue(mockToken);
    expect(service.extractRoleFromToken()).toEqual('admin');
  });
});