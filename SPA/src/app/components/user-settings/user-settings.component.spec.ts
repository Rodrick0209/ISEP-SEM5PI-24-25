import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
import { UserSettingsComponent } from './user-settings.component';
import { of } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('UserSettingsComponent', () => {
  let component: UserSettingsComponent;
  let fixture: ComponentFixture<UserSettingsComponent>;
  let mockActivatedRoute;

  beforeEach(async () => {
    mockActivatedRoute = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue('test@example.com')
        }
      }
    };

    await TestBed.configureTestingModule({
      imports: [RouterTestingModule, HttpClientTestingModule],
      providers: [
        { provide: ActivatedRoute, useValue: mockActivatedRoute }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UserSettingsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize email from route params', () => {
    expect(component.email).toBe('test@example.com');
  });

  it('should navigate to edit user on editUser call', () => {
    const router = TestBed.inject(Router);
    spyOn(router, 'navigate');

    component.editUser();

    expect(router.navigate).toHaveBeenCalledWith(['/edit', 'test@example.com']);
  });

  it('should navigate to delete user on deleteUser call', () => {
    const router = TestBed.inject(Router);
    spyOn(router, 'navigate');

    component.deleteUser();

    expect(router.navigate).toHaveBeenCalledWith(['/delete', 'test@example.com']);
  });
});