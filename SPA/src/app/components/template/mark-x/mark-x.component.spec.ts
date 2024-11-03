import { ComponentFixture, TestBed } from '@angular/core/testing';

import { MarkXComponent } from './mark-x.component';

describe('MarkXComponent', () => {
  let component: MarkXComponent;
  let fixture: ComponentFixture<MarkXComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [MarkXComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(MarkXComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
